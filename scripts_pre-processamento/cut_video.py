from moviepy.video.io.ffmpeg_tools import ffmpeg_extract_subclip
ffmpeg_extract_subclip("avUniversidade.avi", (0)*60, (30)*60, targetname="avUniversidade_00_30.avi")
ffmpeg_extract_subclip("avUniversidade.avi", (30)*60, (60)*60, targetname="avUniversidade_30_60.avi")
ffmpeg_extract_subclip("avUniversidade.avi", (60)*60, (90)*60, targetname="avUniversidade_60_90.avi")
ffmpeg_extract_subclip("avUniversidade.avi", (90)*60, (120)*60, targetname="avUniversidade_90_120.avi")
ffmpeg_extract_subclip("avUniversidade.avi", (120)*60, (150)*60, targetname="avUniversidade_120_150.avi")
ffmpeg_extract_subclip("avUniversidade.avi", (150)*60, (180)*60, targetname="avUniversidade_150_180.avi")

#!ffmpeg -y -loglevel panic -i avAbolicao_33_63.avi -filter:v fps=10 avAbolicao_33_63.mp4