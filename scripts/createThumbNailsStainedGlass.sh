
for file in *; do 
    if [ -f "$file" ]; then 
        inkscape  -w 400 -h 400 "$file" -e "thumbnails/${file%.svg}.png" 
    fi 
done
