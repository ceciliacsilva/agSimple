ffmpeg -framerate 4 -i simul1/geracao%000d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p output.mp4
