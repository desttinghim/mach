const std = @import("std");
const Mode = @import("main.zig").Mode;
const DeviceOptions = @import("main.zig").DeviceOptions;
const DeviceProperties = @import("main.zig").DeviceProperties;
const Format = @import("main.zig").Format;
const c = @import("soundio").c;
const Aim = @import("soundio").Aim;
const SoundIo = @import("soundio").SoundIo;
const SoundIoFormat = @import("soundio").Format;
const SoundIoDevice = @import("soundio").Device;
const SoundIoInStream = @import("soundio").InStream;
const SoundIoOutStream = @import("soundio").OutStream;

const SoundIoStream = union(Mode) {
    input: SoundIoInStream,
    output: SoundIoOutStream,
};

const Audio = @This();

pub const DataCallback = if (@import("builtin").zig_backend == .stage1)
    fn (device: *Device, user_data: ?*anyopaque, buffer: []u8) void
else
    *const fn (device: *Device, user_data: ?*anyopaque, buffer: []u8) void;

var seconds_offset: f32 = 0;

pub const Device = struct {
    properties: Properties,

    // Internal fields.
    handle: SoundIoStream,
    data_callback: ?DataCallback = null,
    user_data: ?*anyopaque = null,
    planar_buffer: [512000]u8 = undefined,
    started: bool = false,

    pub const Options = DeviceOptions;
    pub const Properties = DeviceProperties;

    pub fn deinit(self: *Device, allocator: std.mem.Allocator) void {
        switch (self.handle) {
            .input => |d| d.deinit(),
            .output => |d| d.deinit(),
        }
        allocator.destroy(self);
    }

    pub fn setCallback(self: *Device, callback: DataCallback, data: *anyopaque) void {
        self.data_callback = callback;
        self.user_data = data;
        switch (self.handle) {
            .input => |_| @panic("input not supported yet"),
            .output => |d| {
                // TODO(sysaudio): support other formats
                d.setFormat(.float32LE);

                d.setWriteCallback((struct {
                    fn cCallback(
                        c_outstream: ?[*]c.SoundIoOutStream,
                        frame_count_min: c_int,
                        frame_count_max: c_int,
                    ) callconv(.C) void {
                        _ = frame_count_min;
                        const outstream = SoundIoOutStream{ .handle = @ptrCast(*c.SoundIoOutStream, c_outstream) };
                        const layout = outstream.layout();
                        const float_sample_rate = outstream.sampleRate();
                        const seconds_per_frame = 1.0 / @intToFloat(f32, float_sample_rate);
                        var frames_left = frame_count_max;

                        while (frames_left > 0) {
                            var frame_count = frames_left;

                            var areas: [*]c.SoundIoChannelArea = undefined;
                            outstream.beginWrite(
                                @ptrCast([*]?[*]c.SoundIoChannelArea, &areas),
                                &frame_count,
                            ) catch |err| std.debug.panic("write failed: {s}", .{@errorName(err)});

                            if (frame_count == 0) break;

                            const pitch = 440.0;
                            const radians_per_second = pitch * 2.0 * std.math.pi;
                            var frame: c_int = 0;
                            while (frame < frame_count) : (frame += 1) {
                                const sample = std.math.sin((seconds_offset + @intToFloat(f32, frame) *
                                    seconds_per_frame) * radians_per_second);
                                {
                                    var channel: usize = 0;
                                    while (channel < @intCast(usize, layout.channelCount())) : (channel += 1) {
                                        const channel_ptr = areas[channel].ptr;
                                        const sample_ptr = &channel_ptr[@intCast(usize, areas[channel].step * frame)];
                                        @ptrCast(*f32, @alignCast(@alignOf(f32), sample_ptr)).* = sample;
                                    }
                                }
                            }
                            seconds_offset += seconds_per_frame * @intToFloat(f32, frame_count);
                            outstream.endWrite() catch |err| std.debug.panic("end write failed: {s}", .{@errorName(err)});
                            frames_left -= frame_count;
                        }
                    }
                }).cCallback);
            },
        }
    }

    pub fn pause(device: *Device) Error!void {
        if (!device.started) return;
        return (switch (device.handle) {
            .input => |d| d.pause(true),
            .output => |d| d.pause(true),
        }) catch |err| {
            return switch (err) {
                error.OutOfMemory => error.OutOfMemory,
                else => @panic(@errorName(err)),
            };
        };
    }

    pub fn start(device: *Device) Error!void {
        // TODO(sysaudio): after pause, may need to call d.pause(false) instead of d.start()?
        if (!device.started) {
            device.started = true;
            return (switch (device.handle) {
                .input => |d| d.start(),
                .output => |d| d.start(),
            }) catch |err| {
                return switch (err) {
                    error.OutOfMemory => error.OutOfMemory,
                    else => @panic(@errorName(err)),
                };
            };
        } else {
            return (switch (device.handle) {
                .input => |d| d.pause(false),
                .output => |d| d.pause(false),
            }) catch |err| {
                return switch (err) {
                    error.OutOfMemory => error.OutOfMemory,
                    else => @panic(@errorName(err)),
                };
            };
        }
    }
};

pub const DeviceIterator = struct {
    ctx: Audio,
    mode: Mode,
    device_len: u16,
    index: u16,

    pub fn next(self: *DeviceIterator) IteratorError!?DeviceOptions {
        if (self.index < self.device_len) {
            const device_desc = switch (self.mode) {
                .input => self.ctx.handle.getInputDevice(self.index) orelse return null,
                .output => self.ctx.handle.getOutputDevice(self.index) orelse return null,
            };
            self.index += 1;
            return DeviceOptions{
                .mode = switch (@intToEnum(Aim, device_desc.handle.aim)) {
                    .input => .input,
                    .output => .output,
                },
                .is_raw = device_desc.handle.is_raw,
                .id = device_desc.id(),
                .name = device_desc.name(),
            };
        }
        return null;
    }
};

// TODO(sysaudio): standardize errors across backends
pub const IteratorError = error{OutOfMemory};
pub const Error = error{
    OutOfMemory,
    InvalidDeviceID,
    InvalidParameter,
    NoDeviceFound,
    AlreadyConnected,
    CannotConnect,
    UnsupportedOS,
    UnsupportedBackend,
    DeviceUnavailable,
    Invalid,
    OpeningDevice,
    BackendDisconnected,
    SystemResources,
    NoSuchClient,
    IncompatibleBackend,
    IncompatibleDevice,
    InitAudioBackend,
    NoSuchDevice,
    BackendUnavailable,
    Streaming,
    Interrupted,
    Underflow,
    EncodingString,
};

handle: SoundIo,

pub fn init() Error!Audio {
    var self = Audio{
        .handle = try SoundIo.init(),
    };
    self.handle.connect() catch |err| {
        return switch (err) {
            error.SystemResources, error.NoSuchClient => error.CannotConnect,
            error.Invalid => error.AlreadyConnected,
            error.OutOfMemory => error.OutOfMemory,
            else => unreachable,
        };
    };
    self.handle.flushEvents();
    return self;
}

pub fn deinit(self: Audio) void {
    self.handle.deinit();
}

pub fn waitEvents(self: Audio) void {
    self.handle.waitEvents();
}

pub fn requestDevice(self: Audio, allocator: std.mem.Allocator, options: DeviceOptions) Error!*Device {
    var sio_device: SoundIoDevice = undefined;

    if (options.id) |id| {
        if (options.is_raw == null)
            return error.InvalidParameter;

        sio_device = switch (options.mode) {
            .input => self.handle.getInputDeviceFromID(id, options.is_raw.?),
            .output => self.handle.getOutputDeviceFromID(id, options.is_raw.?),
        } orelse {
            return if (switch (options.mode) {
                .input => self.handle.inputDeviceCount().?,
                .output => self.handle.outputDeviceCount().?,
            } == 0)
                error.NoDeviceFound
            else
                error.DeviceUnavailable;
        };
    } else {
        const id = switch (options.mode) {
            .input => self.handle.defaultInputDeviceIndex(),
            .output => self.handle.defaultOutputDeviceIndex(),
        } orelse return error.NoDeviceFound;
        sio_device = switch (options.mode) {
            .input => self.handle.getInputDevice(id),
            .output => self.handle.getOutputDevice(id),
        } orelse return error.DeviceUnavailable;
    }

    const handle = switch (options.mode) {
        .input => SoundIoStream{ .input = try sio_device.createInStream() },
        .output => SoundIoStream{ .output = try sio_device.createOutStream() },
    };

    switch (handle) {
        .input => |d| try d.open(),
        .output => |d| try d.open(),
    }

    const device = try allocator.create(Device);
    switch (handle) {
        .input => |d| d.handle.userdata = device,
        .output => |d| d.handle.userdata = device,
    }

    // TODO(sysaudio): handle big endian architectures
    const format: Format = switch (handle) {
        .input => |d| switch (@intToEnum(SoundIoFormat, d.handle.format)) {
            .U8 => .U8,
            .S16LE => .S16,
            .S24LE => .S24,
            .S32LE => .S32,
            .float32LE => .F32,
            else => return error.InvalidParameter,
        },
        .output => |d| switch (@intToEnum(SoundIoFormat, d.handle.format)) {
            .U8 => .U8,
            .S16LE => .S16,
            .S24LE => .S24,
            .S32LE => .S32,
            .float32LE => .F32,
            else => return error.InvalidParameter,
        },
    };

    // TODO(sysaudio): Get the device name. Calling span or sliceTo on the name is causing segfaults on NixOS
    // const name_ptr = switch(handle) {
    //     .input => |d| d.handle.name,
    //     .output => |d| d.handle.name,
    // };
    // const name = std.mem.sliceTo(name_ptr, 0);

    var properties = DeviceProperties{
        .is_raw = options.is_raw orelse false,
        .format = format,
        .mode = options.mode,
        .id = std.mem.span(sio_device.handle.id),
        .name = "",
        .channels = @intCast(u8, switch (handle) {
            .input => |d| d.layout().channelCount(),
            .output => |d| d.layout().channelCount(),
        }),
        .sample_rate = @intCast(u32, switch (handle) {
            .input => |d| d.sampleRate(),
            .output => |d| d.sampleRate(),
        }),
    };

    device.* = .{
        .properties = properties,
        .handle = handle,
    };
    return device;
}

pub fn outputDeviceIterator(self: Audio) DeviceIterator {
    return .{
        .ctx = self,
        .mode = .output,
        .device_len = self.handle.outputDeviceCount() orelse 0,
        .index = 0,
    };
}

pub fn inputDeviceIterator(self: Audio) DeviceIterator {
    return .{
        .ctx = self,
        .mode = .input,
        .device_len = self.handle.inputDeviceCount() orelse 0,
        .index = 0,
    };
}
