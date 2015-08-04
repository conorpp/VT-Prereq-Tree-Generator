
 (* Fetch class listings for  a paricular semester/department *)
 baseurl = "https://banweb.banner.vt.edu/ssb/prod/";

  (* avoid xxs... *)
 escapeScripts[y_] := StringReplace[y, "SCRIPT"->"script"];

 getTimeTable[sub_, sem_] := escapeScripts[ URLFetch[baseurl<>"HZSKVTSC.P_ProcRequest/","Parameters"->{ "CAMPUS"->"0",  "TERMYEAR" -> sem,  "CORE_CODE"->"AR%","subj_code"->sub, "SCHDTYPE"->"%", "CRSE_NUMBER"->"", "crn"->"",  "open_only"->"", "inst_name"->"", "BTN_PRESSED"->"FIND class sections" }] ]; 

 
  (* Pull the links for each class *)
buildLink[csrn_,subj_] := baseurl<>"HZSKVTSC.P_ProcCrseText?SUBJ_IN="<>subj<>"&CRSE_IN="<> csrn;
 getLinks[html_,subj_] := (buildLink[#,subj ])& /@StringCases[html, RegularExpression["HZSKVTSC[.]P_ProcComments[?]CRN=[0-9]+(.|\\s)*?CRSE=([0-9]+)"]->"$2"];
 


(* Pulling class details and parsing the relevant info *)
(*
[[1]] = Class prereqs CRSE list
    *[[1]]  Prereq csre
    *[[2]] Prereq subj
[[2]] = Class Name
[[3]] = Class Link
[[4]] = meh
[[5]] = Class CRSE
[[6]] = Class Subject
*)
matchCrse[x_] := StringCases[x, RegularExpression["CRSE_IN=([0-9]+)"]->"$1"];
matchSubj[x_] := StringCases[x, RegularExpression["SUBJ_IN=([A-Za-z]+)"]->"$1"];

 getClassInfo1[x_] := (y=escapeScripts[URLFetch[x]]; {StringCases[y,RegularExpression["Prerequisites:[\\s]?</B></SPAN>((\\s|.)*?)</FONT>"]->"$1"][[1]], StringCases[y, RegularExpression["<TITLE>((\\s|.)*?)</TITLE>"]->"$1"][[1]]});
 getClassInfo2[x_] := (y=getClassInfo1[x]; {StringReplace[y[[1]], RegularExpression["\\s"]->""],y[[2]]});
 getClassInfo3[x_] := (y=getClassInfo2[x]; {y[[1]],StringReplace[y[[2]], "-"->"\n"], x, "meh",  matchCrse[x][[1]]});
 getClassInfo4[x_] := (y=getClassInfo3[x]; {y[[1]],y[[2]],y[[3]],y[[4]], y[[5]],  matchSubj[x][[1]]});
 getClassInfo5[x_] := (y=getClassInfo4[x]; {StringCases[y[[1]], RegularExpression["<A((\\s|.)*?)</A>"]->"$1"],y[[2]],y[[3]],y[[4]], y[[5]], y[[6]]});
 getClassInfo6[x_] := (y=getClassInfo5[x]; {({ matchCrse[#][[1]], matchSubj[#][[1]] } &/@ y[[1]]),y[[2]],y[[3]],y[[4]], y[[5]], y[[6]]});
getClasses[sub_, sem_] := (getClassInfo6[#])& /@ getLinks[  getTimeTable[sub, sem], sub  ];

(* Get a list of classes that are prereqs for the given class *)

getDepends[x_]:=( getClassInfo6[#] )& /@ ((  buildLink[#[[1]], #[[2]]])& /@ x[[1]]);

getPrereqCrses[a_] := Flatten[ (#[[1]] )&/@ a,1];
getCurCrses[a_] :=  ({#[[5]],#[[6]]} )&/@ a
getMissingClasses[a_] := Complement[getPrereqCrses[a],getCurCrses [a]];

getAllDepends[x_] := (all=getDepends[x]; all);

(* divide into classes with and w/o prereqs *)

divideClasses[x_] := ( y= getClasses[x, "201509"]; While[(miss = getMissingClasses[y];Length[miss]>0 ), y=Union[y,getDepends[{miss}]] ] ; {Select[y,  Length[#[[1]]] > 0 &], Select[y,  Length[#[[1]]] == 0 &]}); 

(* Make edges between a given class and it's prereqs *)
makeEdges[x_] := (( x[[2]]->#[[2]] )& /@ getAllDepends[x]); 

(* Get all nodes for a graph given a department code *)
getNodes[x_] := DeleteDuplicates[Flatten[(cl = divideClasses[x];{(makeEdges[#])&/@  DeleteDuplicates[cl[[1]]],(#[[2]]->"None")&/@  DeleteDuplicates[cl[[2]]]}) ]];


(* Get the graph *)
getClassGraph[depart_] := LayeredGraphPlot[(n=getNodes[depart]; Print[Length[n]]; n), DirectedEdges -> False,VertexLabeling -> True, ImageSize->Log2[Length[n]]* If[Length[n] > 100, If[Length[n]>200, If[Length[n]>300, 1500, 1300],1100 ], 600]];


departmentList = {"AAEC","ACIS","AFST","AHRM","AINS","ALCE","ALS","AOE","APS","APSC","ARBC","ARCH","ART","AS","ASPT","AT","BC","BCHM","BIOL","BIT","BMES","BMSP","BMVS","BSE","BTDM","BUS","CEE","CHE","CHEM","CHN","CINE","CLA","CMDA","CNST","COMM","COS","CRIM","CS","CSES","DASC","ECE","ECON","EDCI","EDCO","EDCT","EDEL","EDEP","EDHE","EDIT","EDP","EDRE","EDTE","ENGE","ENGL","ENGR","ENSC","ENT","ESM","FA","FIN","FIW","FL","FOR","FR","FST","GBCB","GEOG","GEOS","GER","GIA","GR","GRAD","HD","HEB","HIST","HNFE","HORT","HTM","HUM","IDS","IS","ISC","ISE","ITAL","ITDS","JPN","JUD","LAHS","LAR","LAT","LDRS","MACR","MATH","ME","MGT","MINE","MKTG","MN","MS","MSE","MTRG","MUS","NANO","NEUR","NR","NSEG","PAPA","PHIL","PHS","PHYS","PORT","PPWS","PSCI","PSVP","PSYC","REAL","RLCL","RTM","RUS","SBIO","SOC","SPAN","SPIA","STAT","STL","STS","SYSB","TA","TBMH","UAP","UH","UNIV","VM"};


CloudDeploy[
FormFunction["Department" -> departmentList, getClassGraph[#Department] &,
"JPG" , AppearanceRules->{"Title"->"Virginia Tech Class Prerequisite Tree Generator", "Description"->"Pick a department to generate a visual tree graph of the class prereqs.  Please be patient, larger departments will take a minute."}],
"Class Graph", Permissions -> "Public"]



