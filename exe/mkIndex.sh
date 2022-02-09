#!/bin/sh

echo '<html><head><title>SoccerFun Tournament</title></head><body><table border="1" rules="all">'

echo "<tr>"
for i in teams/*; do
	echo "<td>`basename $i`</td>"
done
echo "<td></td>"
echo "</tr>"

for i in teams/*; do
	echo "<tr>"
	for j in teams/*; do
		game=`basename $j`-`basename $i`
		echo "<td>"
		if [ -f "tapes/$game.log" ]; then
			if [ 0`stat -c%s "tapes/$game.log"` -lt 10 ]; then
				[ -s "tapes/$game.sft" ] && echo "<a href=\"$game.sft\">"
				cat "tapes/$game.log"
				[ -s "tapes/$game.sft" ] && echo "</a>"
			else
				echo "<a href=\"$game.log\">error</a>"
			fi
		fi
		echo "</td>"
	done;
	echo "<td>`basename $i`</td>"
	echo "</tr>"
done

echo '</table></title></head><body><table></html>'
