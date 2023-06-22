#!/bin/bash

tmpdir="selfcheck_tmp"
test_files=("q1" "q2")
required_files=("hw6_q1.pl" "hw6_q2.pl" "dry.pdf")

hw6="https://raw.githubusercontent.com/AdiHarif/236319-Spr-2023/master/HW/Homework6/"

if [ -z "$1" ]; then
	echo "Usage: ./"$( basename "$0" )" <your submission zip file> [directory with previous solutions]"
	exit
fi

if [ -z "$2" ]; then
	echo "Warning - no directory with previous solutions was given (ignore in case you do not use your previous solution)."
	echo "Usage: ./"$( basename "$0" )" <your submission zip file> [directory with previous solutions]"
fi


if [ ! -f "$1" ]; then
	echo "Submission zip file not found!"
	exit
fi

rm -rf "$tmpdir" &> /dev/null
if [ -d "$tmpdir" ]
	then
		echo "Cannot clear tmp directory. Please delete '$tmpdir' manually and try again"
		exit
fi
mkdir "$tmpdir" &> /dev/null

yes | apt update &> /dev/null
yes | apt install wget &> /dev/null
yes | apt install zip &> /dev/null

unzip "$1" -d "$tmpdir" &> /dev/null
if [[ $? != 0 ]]; then
	echo "Unable to unzip submission file!"
	exit
fi

cd "$tmpdir"
for f in "${required_files[@]}"
do
	if [ ! -f  $f ]; then
		echo "File $f not found!"
		exit
	fi
done

if [ $( ls | wc -l ) != ${#required_files[@]} ]; then
	echo "There are too many files in the submission"
	exit
fi

cp ../$2/* . &> /dev/null

for test in "${test_files[@]}"
do
	wget "$hw6$test.in" "$hw6$test.expected"  &> /dev/null
	sleep 3
	if [ ! -f "$test.in" ] || [ ! -f "$test.expected" ]; then
		echo "Unable to download test $test!"
		exit
	fi
	swipl -q hw6_$test.pl < $test.in &> $test.rawout
	sed '1,/===TEST START===/{N;d}' $test.rawout > $test.out
	diff -wB $test.out $test.expected
	if [[ $? != 0 ]]; then
		echo "Failed test $test!"
		exit
	fi
done

cd - &> /dev/null
rm -rf "$tmpdir"

echo "Ok to submit :)"
exit
