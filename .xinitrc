# for dwm statusbar 
#
compton &
feh --bg-fill ~/Pictures/Wallpapers/mountain.jpg
# bash ~/scripts/xsetroot.sh

while true; do
	DATETIME=$( date +"%F %R" )
	VOLUME=$( amixer sget Master | grep -e 'Front Left:' | \
    	sed 's/[^\[]*\[\([0-9]\{1,3\}%\).*\(on\|off\).*/\2 \1/' | sed 's/off/M/' | sed 's/on //' )
	BATTERYSTATE=$( acpi -b | awk '{ split($5,a,":"); print substr($3,0,2), $4, "["a[1]":"a[2]"]" }' | tr -d ',' )
	WIFI=$( iwgetid -r )
	xsetroot -name "  ${WIFI} |  ${VOLUME} |  ${BATTERYSTATE} |  ${DATETIME}"
	# xsetroot -name "NET: ${WIFI} | VOL: ${VOLUME} | BAT: ${BATTERYSTATE} | DAY: ${DATETIME}"
   	sleep 10s 
done &
