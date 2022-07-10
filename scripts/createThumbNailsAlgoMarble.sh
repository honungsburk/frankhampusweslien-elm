for file in *; do 
    if [ -f "$file" ]; then
    convert -thumbnail 400 "$file" "${file%_Thumb_Nail.png}"
    fi 
done


