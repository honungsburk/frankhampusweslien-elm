for file in *; do 
    if [ -f "$file" ]; then
        convert  -crop 1080x1080+460+0 -resize 400x400 "$file" "${file%_Thumb_Nail.png}"
    fi 
done



