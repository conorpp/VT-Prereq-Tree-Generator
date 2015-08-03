# VT-Prereq-Tree-Generator
Wolfram Language source for generating a tree graph of classes/prereqs for a department.  
It's necessary because it's hard sometimes to figure out prereqs for courses
quickly on VT's timetable of classes.


This is my first go at a functional language.  I'd appreciate any feedback on my style.

Go to the generator at this link:
    
https://www.wolframcloud.com/objects/user-f5b01625-1be5-47ac-8cd6-538aa2ec1ae2/Class%20Graph

Pick a department and see the generator graph.  Larger departments will take time.  Try LAT department for a small one.

# How it works

It scrapes the html from the timetable endpoint at https://banweb.banner.vt.edu/ssb/prod/HZSKVTSC.P_DispRequest?term=09&year=2015

It follows all the prereq links and then makes a list of nodes/edges to pass to a Wolfram
function for generating the graph.


