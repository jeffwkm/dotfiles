# vim: set ft=python:

# see the README at https://gist.github.com/phiresky/4bfcfbbd05b3c2ed8645
# source: https://github.com/mpv-player/mpv/issues/2149
# source: https://github.com/mpv-player/mpv/issues/566
# source: https://github.com/haasn/gentoo-conf/blob/nanodesu/home/nand/.mpv/filters/mvtools.vpy

import vapoursynth

import os

core = vapoursynth.core
core.std.LoadAllPlugins(
    # "/etc/profiles/per-user/jeff/lib/vapoursynth"
    os.environ["VAPOURSYNTH_MVTOOLS_LIB"]
)
# ref: http://avisynth.org.ru/mvtools/mvtools2.html#functions
# default is 400, less means interpolation will only happen when it will work well
ignore_threshold = 200 # 140
# if n% of blocks change more than threshold then don't interpolate at all (default is 51%)
scene_change_percentage = 15 # 15

# Interpolating to fps higher than 60 is too CPU-expensive, smoothmotion can handle the rest.
# while (dst_fps > 60):
#    dst_fps /= 2

if "video_in" in globals():
    # realtime
    clip = video_in
    src_fps_num = int(container_fps * 1e8)
    src_fps_den = int(1e8)
    clip = core.std.AssumeFPS(clip, fpsnum=src_fps_num, fpsden=src_fps_den)
else:
    # run with vspipe
    clip = core.ffms2.Source(source=in_filename)

clip_fps = round(clip.fps_num / clip.fps_den)

# resolution in megapixels. 1080p ≈ 2MP, 720p ≈ 1MP
mpix = clip.width * clip.height / 1000000

# Use faster parameters for large videos
use_fast_mode = mpix > 2.5 or clip_fps >= 50

use_flow_fps = not use_fast_mode
# use_flow_fps = False

# define global variables for set_dst_fps
dst_fps = 0.0
dst_fps_num = 0
dst_fps_den = 0

def set_dst_fps(double_dps:bool = False):
    global dst_fps, dst_fps_num, dst_fps_den
    # dst_fps = display_fps
    dst_fps = 60.0
    if double_dps:
        dst_fps = 120.0
    dst_fps_num = int(dst_fps * 1e4)
    dst_fps_den = int(1e4)

color_on = "\x1b[38;5;13m" # magenta
highlight_on = "\x1b[38;5;10m" # green
color_off = "\x1b[0m"

msg_prefix = "[motioninterpolation.vpy]"
msg_prefix += " ::"

# Don't interpolate if too resource intensive
if not (mpix > 10 or clip_fps > 50):
    analParams = {
        "overlap": 0,
        "search": 3,
        "truemotion": True,
        #'chrome': True,
        "blksize": 8,
        #'searchparam':1
    }
    blockParams = {
        "thscd1": ignore_threshold,
        "thscd2": int(scene_change_percentage * 255 / 100),
    }

    if use_fast_mode:
        analParams["search"] = 0
        analParams["blksize"] = 32
        blockParams["mode"] = 0
    elif not use_flow_fps:
        blockParams["mode"] = 3

    # set_dst_fps(not use_fast_mode)
    set_dst_fps(False)

    print(
        "\n{}{} {} [{} -> {} fps] {}{}".format(
            color_on,
            msg_prefix,
            "FlowFPS" if use_flow_fps else "BlockFPS",
            highlight_on + str(clip_fps) + color_on,
            highlight_on + str(round(dst_fps)) + color_on,
            "[fast mode]" if use_fast_mode else "",
            color_off,
        ),
    )

    sup = core.mv.Super(clip, pel=2)
    bvec = core.mv.Analyse(sup, isb=True, **analParams)
    fvec = core.mv.Analyse(sup, isb=False, **analParams)

    f = {True: core.mv.FlowFPS, False: core.mv.BlockFPS}[use_flow_fps]
    clip = f(clip, sup, bvec, fvec, num=dst_fps_num, den=dst_fps_den, **blockParams)
    clip.set_output()
else:
    print(
        "\n{}{} skipping{}".format(
            color_on,
            msg_prefix,
            color_off,
        ),
    )
    video_in.set_output()
