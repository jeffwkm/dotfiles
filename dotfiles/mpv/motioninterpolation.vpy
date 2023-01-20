# vim: set ft=python:

# see the README at https://gist.github.com/phiresky/4bfcfbbd05b3c2ed8645
# source: https://github.com/mpv-player/mpv/issues/2149
# source: https://github.com/mpv-player/mpv/issues/566
# source: https://github.com/haasn/gentoo-conf/blob/nanodesu/home/nand/.mpv/filters/mvtools.vpy

import vapoursynth

core = vapoursynth.core
core.std.LoadAllPlugins(
    "/nix/var/nix/profiles/per-user/jeff/home-manager/home-path/lib/vapoursynth"
)
# ref: http://avisynth.org.ru/mvtools/mvtools2.html#functions
# default is 400, less means interpolation will only happen when it will work well
ignore_threshold = 200  # 140
# if n% of blocks change more than threshold then don't interpolate at all (default is 51%)
scene_change_percentage = 15

# dst_fps = display_fps
dst_fps = 60
# Interpolating to fps higher than 60 is too CPU-expensive, smoothmotion can handle the rest.
# while (dst_fps > 60):
#    dst_fps /= 2

if "video_in" in globals():
    # realtime
    clip = video_in
    # Needed because clip FPS is missing
    src_fps_num = int(container_fps * 1e8)
    src_fps_den = int(1e8)
    clip = core.std.AssumeFPS(clip, fpsnum=src_fps_num, fpsden=src_fps_den)
else:
    # run with vspipe
    clip = core.ffms2.Source(source=in_filename)
    dst_fps = float(dst_fps)

clip_fps = int(clip.fps_num / clip.fps_den)

# resolution in megapixels. 1080p ≈ 2MP, 720p ≈ 1MP
mpix = clip.width * clip.height / 1000000

# Don't interpolate if too resource intensive
ignore = mpix > 10 or clip_fps > 50
# Use faster parameters for large videos
fast_mode = mpix > 2.5 or clip_fps >= 50

flow_fps = not fast_mode
# flow_fps = False

if not ignore:
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

    if fast_mode:
        analParams["search"] = 0
        analParams["blksize"] = 16
        blockParams["mode"] = 0

    if (not fast_mode) and (not flow_fps):
        blockParams["mode"] = 3

    dst_fps_num = int(dst_fps * 1e4)
    dst_fps_den = int(1e4)
    print(
        "Reflowing from {} fps to {} fps (quality = {})".format(
            clip_fps, dst_fps, {True: "low", False: "high"}[fast_mode]
        )
    )

    sup = core.mv.Super(clip, pel=2)
    bvec = core.mv.Analyse(sup, isb=True, **analParams)
    fvec = core.mv.Analyse(sup, isb=False, **analParams)

    f = {True: core.mv.FlowFPS, False: core.mv.BlockFPS}[flow_fps]
    clip = f(clip, sup, bvec, fvec, num=dst_fps_num, den=dst_fps_den, **blockParams)

clip.set_output()
