BeginPackage["MathTeX`" ];
PrintListing::usage = "True or False";
TeX::usage = "TeX prints out a TeX version of expression.\n";
ReInterpret::usage = "ReInterpret";
FigSave::usage = "FigSave[filename,plot] saves plott";
FigInsert::usage = "FigInsert[filename,width,caption] width number in cminserts fig"; 
Zellipsis::usage = "";
Typeset::usage = "Typeset[a_,b___,Sub[m_], Sup[n_],g___ ] := Typeset[a,b,Sup[n], Sub[m],g];";
$thisfile::usage = "ThisFile";
Sub::usage = "Subscript in TeX";
TeXVersion::usage = "2008-12-02";
Sup::usage = "Superscript in TeX";
UsageKeys::usage = "UsageKeys";
sum::usage = "sum";
plus::usage = "plus";
ssum::usage = "ssum";
cols::usage = "cols";
frac::usage = "frac";
cfrac::usage = "cfrac";
ret::usage = "ret";
slist::usage = "slist";
TeXDependence::usage = "TeXDependence";
TeXFormat::usage = "TeXFunc";
times::usage = "times";
Paren::usage = "put explicit parenthesis";
dot::usage = "dot";
FileIsOpen::usage = "FileIsOpen usage is ";
TeXGraphics::usage = "TeXGraphics";
TeXStrip::usage = "TeXStrip[name] strips tex commands from name.mtex and puts in name.m";
LISTING::usage ="LISTING";
DOCUMENT::usage ="DOCUMENT";
INLINE::usage = "INLINE";
CINLINE::usage = "CINLINE";
PINLINE::usage = "PINLINE";
USAGE::usage = "USAGE";
NINLINE::usage = "NINLINE";
EQUATION::usage = "EQUATION";
PEQUATION::usage = "EQUATION";
CEQUATION::usage = "EQUATION";
$errormsg::usage = "$errormsg";

(* TeXPrint[a_] := Module[{},
        oldpagewidth = Options[$Output, PageWidth ];
        SetOptions[$Output, PageWidth -> Infinity];
        Print[TeX[a]];
        SetOptions[$Output, oldpagewidth];
        ];
*)

Begin["`Private`"];
PrintListing = True;
$thisfile = "defaultTHISFILE";
CURRENTFile = "defaultCURRENTFILE";
$lastlineno = 0;




ReInterpret[a_Real] := Print["Reinterpret at real"];
ReInterpret[ins_?StringQ ] := Module[{str,head,fun,tail,instring},
	instring = StripBlanks[ ins ];
	head = ""; tail = ""; str = "";
	fun = "NOTHING";
	fun = Identity;
	(* Print["Reinterpret", ToString[ins] ]; *)
	If[ StringLength[ instring ] >= 8 ,
		head = StringTake[instring, 4];
		tail = StringTake[instring, -4];
		str = StringDrop[StringDrop[instring, -4 ],4],
	   str = instring ];
	If[ StringMatchQ[instring, RegularExpression["^[A-z0-9][A-z0-9]*::usage.*"]], 
		fun = USAGE; str = instring]; 
	If[ StringMatchQ[head, "((((" ] && 
		StringMatchQ[tail,"))))"] , fun =  LISTING ]; 
	If[ StringMatchQ[head, "(\\*((" ] && 
		StringMatchQ[tail,"))\\*)"] , fun =  LISTING ]; 
	If[ StringMatchQ[head, RegularExpression["\\(\\*\\*\\s"] ] && 
		StringMatchQ[tail,RegularExpression["\\s\\*\\*\\)"]] , fun =  DOCUMENT ];  
	If[ StringMatchQ[head, RegularExpression["\\(\\*\\$\\s"] ] && 
		StringMatchQ[tail,RegularExpression["\\s\\$\\*\\)"]] , fun =  INLINE];  
	If[ StringMatchQ[head, RegularExpression["\\(\\*\\$\\s"] ] && 
		StringMatchQ[tail,RegularExpression["\\.\\$\\*\\)"]] , fun =  PINLINE];  
	If[ StringMatchQ[head, RegularExpression["\\(\\*\\$\\s"] ] && 
		StringMatchQ[tail,RegularExpression["\\,\\$\\*\\)"]] , fun =  CINLINE];  
	If[ StringMatchQ[head, RegularExpression["\\(\\*\\!\\s"] ] && 
		StringMatchQ[tail,RegularExpression["\\s\\!\\*\\)"]] , fun =  NINLINE];  
	If[ StringMatchQ[head, RegularExpression["\\(\\*\\[\\s"] ] && 
		StringMatchQ[tail,RegularExpression["\\s\\]\\*\\)"]] , fun =  EQUATION];  
	If[ StringMatchQ[head, RegularExpression["\\(\\*\\[\\s"] ] && 
		StringMatchQ[tail,RegularExpression["\\.\\]\\*\\)"]] , fun =  PEQUATION];  
	If[ StringMatchQ[head, RegularExpression["\\(\\*\\[\\s"] ] && 
		StringMatchQ[tail,RegularExpression["\\,\\]\\*\\)"]] , fun =  CEQUATION];  
	fun[ str ];
	Return[""];
	];

GetFirstComment[str_] := Module[{m1,m2,comment},
        m1 = StringPosition[str,"(*",1];
        m2 = StringPosition[str,"*)",1];
        If[ !MatchQ[ m1, {}] && !MatchQ[ m2, {} ] && m2[[1,1]] - m1[[1,2]] >= 1,
        comment = StringTake[ str, { m1[[1,2]] +1, m2[[1,1]] -1 }];
        comment = StringReplace[ comment, { RegularExpression["^\\s*"]:> "",
                RegularExpression["\\s*$"]:> ""} ],
                comment = "";
         ];
	comment = StringReplace[comment,{"#" -> "\\#"}];
        Return[ comment]
];

StripBlanks[g_] := Module[{ex},
        ex = g;
        (* While[ StringLength[ ex] > 0 && ( StringTake[ex,1] == " "  ) , ex = StringDrop[ex,1] ];
        While[ StringLength[ ex] > 0 && ( StringTake[ex,-1] == " " ) , ex = StringDrop[ex,-1] ]; *)
	ex = StringReplace[ ex, { RegularExpression["^\\s *" ] -> "" ,
				  RegularExpression["\\s *$" ] -> ""  }];
        Return[ ex ]
        ];

getfiln[ex_] := Module[{filn},
        filn = StringReplace[ex ,{RegularExpression["\\s*"] -> "" } ];
        filn = StringSplit[filn ,{"="} ][[2]];
	Print["In getfiln, ex, filn  = ", ex, filn ];
        Return[ filn]
        ];

newfile = True;

DOCUMENT[g_] :=  Module[{ex,str},
 ex = g;
 If[ StringMatchQ[ex,RegularExpression[".*FILE.*=.*"] ],
	Print["dumping into", ex ];
	Unprotect[ CURRENTFile,FileIsOpen];
        CURRENTFile=  getfiln[ ex ];
        (* If[ ValueQ[ $doc], Close[$doc] ];  *)
	Print["CURRENTFile = ", CURRENTFile ];
        If[ MatchQ[ FileIsOpen[ CURRENTFile] ,1 ],
                $doc = OpenAppend[CURRENTFile],
                $doc = OpenWrite[CURRENTFile]
                ];
	Close[$doc];
        FileIsOpen[CURRENTFile] = 1;
        Protect[CURRENTFile, FileIsOpen]; ex = "" ,
    If[ ValueQ[ CURRENTFile],
        	$doc = OpenAppend[CURRENTFile];
		(* If[ newfile , Block[ newfile = False; newstring = "newcommand" ] , newstring = "renewcommand" ]; *)
        	str = ex;
		(* str = "\\"<>newstring<>"{\\thisfile}{CURRENTFILE}\n"<>str; *)
		(* WriteString[$doc,str<>"\n"]; *)
		WriteString[$doc,str];
        	Close[$doc];
        ];
   ];
 Return[""]
];

(*** XXXXXXXXXXXXXXXXXXXXXXXXX  Fix up indexing XXXXXXXXXXXXXXXXXXXXXXXXXX ***)
USAGE[s_] :=  Module[{},
	If[ FileType["usages"] == File ,
		UsageKeys = ReadList["usages"],
		UsageKeys = {};
		];
	keyword = StringReplace[s, { RegularExpression[ "::.*"] -> "" }];
	UsageKeys = Union[ UsageKeys, {keyword}];
	$doc = OpenWrite["usages"];
	Do[ 
		Write[$doc,UsageKeys[[i]]],
			{i,1,Length[ UsageKeys] }];
	Close[$doc];
	$doc = OpenWrite["Tex/ListingIndex.tex"];
	WriteString[$doc,"\\lstset{emph={[3]"];
	str = ( #<>"," & /@ Drop[UsageKeys,-1])<>UsageKeys[[-1]];
	WriteString[$doc, str];
	WriteString[$doc,"},emphstyle={[3]\\color{blue}}}\n"];
	WriteString[$doc,"\\lstset{index=[2]{"];
	str = ( #<>"," & /@ Drop[UsageKeys,-1])<>UsageKeys[[-1]];
	WriteString[$doc, str];
	WriteString[$doc,"}}\n"];
	Close[$doc];
	$doc = OpenAppend[CURRENTFile];
	desc = StringReplace[s, { RegularExpression["^[^=]*= *"] -> "" }];
	(* desc = StringReplace[desc,{"_" -> "\\_"}]; *)
	desc = StringReplace[desc,{"#" -> "\\#"}];
	(* desc = StringReplace[desc,{"^" -> "\\^"}]; *)
	desc = StringReplace[desc,{"\"" -> ""}];
	desc = StringReplace[desc, { RegularExpression[" *; *$"] -> "" }];
	WriteString[$doc,"\\mtexdef{",keyword,"}{", desc ,"}\\index{",keyword,"}\n"];
	Close[$doc];
	Return[];
	];

lshead := "\\renewcommand{\\lastlineno}{"<>ToString[$lastlineno]<>"}\\renewcommand{\\shortcap}{";
lsmid1 := "}\n"<>"\\renewcommand{\\listingcaption}{";
lsmid2 := "}\n"<>"\\lstlistingbegin\n";
lstail := "\n"<> "\\end{lstlisting}\n\\lstlistingend\n"<> "\\renewcommand{\\shortcap}{}\n";
CreateListing := ( x = lshead<>#1<>lsmid1<>#2<>lsmid2<>#3<>lstail  ; 
		$lastlineno += Length[ StringCases[#3 ,"\n"] ]; 
		x ) &;

LISTING[ex_] := Module[{str,comment,ret},
        (* Print["putting stuff into", CURRENTFile ]; *)
	If[ PrintListing,
        $doc = OpenAppend[CURRENTFile];
	comment = GetFirstComment[ex]; 
	If[ StringLength[ comment ] > 60, comment = ""];
	str = StringReplace[ex, {"#" -> "\\#", "@" -> "\\@", "&" -> "\\&" }];
	strs = StringSplit[str,"(* Continued *)" ];
	comments = Table[ If[ MatchQ[i,1], comment, comment<>", continued" ] , {i,1,Length[strs] }];
	str =  StringJoin @@ Table[ 
		CreateListing[comments[[i]],
			$thisfile, 
			strs[[i]]  ],{i,1,Length[strs] }] ;
	str = str<>"\n\\renewcommand{\\filename}{}";
	str = StringReplace[ str, {"\\#" -> "#","\\@" -> "@", "\\&" -> "&"}];
        WriteString[$doc,str];
        Close[$doc]
	];
	Return[""];
        ];


(*
SetAttributes[INLINE,HoldAllComplete];
SetAttributes[EQNS,HoldAllComplete];
SetAttributes[EQUATION,HoldAllComplete];
*)

EQUATION[ex_] := EQNS["\\[\n",ex,"\n\\]\n"];
PEQUATION[ex_] := EQNS["\\[\n",ex,"\n.\\]\n"];
CEQUATION[ex_] := EQNS["\\[\n",ex,"\n,\\]\n"];
INLINE[ex_] := EQNS["$",ex,"$ "];
PINLINE[ex_] := EQNS["$",ex,". $ "];
CINLINE[ex_] := EQNS["$",ex,", $ "];
NINLINE[ex_] := ( Print["DOING NINLINE\n"]; EQNS["",ex,""]  ) ;

(* This is old code

EQNSOLD[head_,ex_,tail_] := Module[{str,comment,ret,substrings,headstring,delimiters,strings,outstring},
        Print["putting eqn stuff into", CURRENTFile ]; 
        $doc = OpenAppend[CURRENTFile];
	substrings = StringSplit[ex,RegularExpression["(=|<>)"]];
	Print["substrings", substrings ]; 
	If[ Length[ substrings] > 1 ,
		headstring = substrings[[1]];
		substrings = Drop[ substrings, 1];
		delimiters = StringCases[ex,RegularExpression["(=|<>)"]];
		strings = Position[ delimiters, "<>"];
		delimiters = StringReplace[ delimiters, { "<>" -> " " } ];
		outstrings = ( ToString[ Evaluate[ ToExpression[ # ] ]] & ) /@ substrings;
		outstring = headstring <>( ( StringJoin[#[[1]] , #[[2]]  ] & ) /@  Transpose[{ delimiters, outstrings }] ),
		outstring = substrings[[1]];
		];
	(* outstring = StringReplace[ outstring, { "\"" -> "", "\\\\" -> "\\" }]; *)
	outstring = StringReplace[ outstring, { "\"" -> ""}];
	WriteString[$doc, head<>outstring<>tail];
        Close[$doc];
	Return[""];
        ];
*)

EQNS[head_,ex_,tail_] := Module[{str,comment,ret,substrings,headstring,delimiters,strings,outstring},
        (* Print["putting eqn stuff into", CURRENTFile ]; *)
        $doc = OpenAppend[CURRENTFile];
	substrings = StringSplit[ex,RegularExpression["(=|<>)"]];
	(* Print["substrings: ", substrings ];  *)
	If[ Length[substrings] != 1, (* This escape for zero length string added 2008-12-02 *)
		headstring = substrings[[1]];
		If[ Length[ StringCases[headstring,"TeX"]]  != 0,
			headstring = ToString[ Evaluate[ToExpression[ headstring ]]];
			];
		substrings = Drop[ substrings, 1];
		delimiters = StringCases[ex,RegularExpression["(=|<>)"]];
		strings = Position[ delimiters, "<>"];
		delimiters = StringReplace[ delimiters, { "<>" -> " " } ];
		outstrings = ( ToString[ Evaluate[ ToExpression[ # ] ]] & ) /@ substrings;
		outstring = ToString[ headstring ] <> If[ Length[ outstrings] > 0,
				( ( StringJoin[#[[1]] , #[[2]]  ] & ) /@  
				Transpose[{ delimiters, outstrings }] ), ""],
		outstring = ToString[ Evaluate[ToExpression[ substrings[[1]] ] ] ]
		];
	(* outstring = StringReplace[ outstring, { "\"" -> ""}]; *)(**  this is strange 2011-08-27 I commented this out ; Stellan **)
	WriteString[$doc, head<>outstring<>tail];
        Close[$doc];
	Return[""];
        ];




TeX[a_,b__] :=  TeX[a] <> TeX[b];
TeX[a_?StringQ] := a;
TeX[- a_?AtomQ] := "-\\,{"<>TeX[a]<>"}";

TeX[ssum[a_?MatrixQ]] := Module[{exp,i,j,len},
        len = Length[ a[[1]] ];
        exp = "\\begin{array}[t]{"<>StringJoin[Table["l",{ Length[a[[1]] ] }]]<>"}\n";
        Do[
        Do[ exp = exp<>TeX[ a[[i,j]] ]<>If[ j < len ," & ",""], {j,1,len}];
        If[ i < Length[a], exp = exp<>" \\; \\;  \\text{+} \\\\\n"],{i,1,Length[a] }];
        exp = exp<>"\n";
        exp = exp<>"\\end{array}";
        Return[exp];
        ];

TeX[cols[a_?MatrixQ]] := Module[{exp,i,j,len},
        len = Length[ a[[1]] ];
        exp = "\\begin{array}[t]{"<>StringJoin[Table["l",{ Length[a[[1]] ] }]]<>"}\n";
        Do[
        Do[ exp = exp<>TeX[ a[[i,j]] ]<>If[ j < len ," & ",""], {j,1,len}];
        If[ i < Length[a], exp = exp<>" \\; \\;  ,   \\\\\n"],{i,1,Length[a] }];
        exp = exp<>"\n";
        exp = exp<>"\\end{array}";
        Return[exp];
        ];



TeX[slist[a_?MatrixQ]] := Module[{exp,i,j,len},
        len = Length[ a[[1]] ];
        exp = "\\begin{array}[t]{"<>StringJoin[Table["l",{1 + Length[a[[1]] ] }]]<>"}\n";
        Do[
        Do[ exp = exp<>TeX[ a[[i,j]] ]<>If[ j < len ," & ",""], {j,1,len}];
        If[ i < Length[a], exp = exp<>" & \\text{,} \\\\\n"],{i,1,Length[a] }];
        exp = exp<>"\n";
        exp = exp<>"\\end{array}";
        Return[exp];
        ];

(* TeX[a_?MatrixQ] := Module[{exp,len,i},
        len = Length[ a[[1]] ];
        exp = "\\left[\\begin{array}{"<>StringJoin[Table["c",{Length[a[[1]] ] }]]<>"}\n";
        Do[
        Do[ exp = exp<>TeX[ a[[i,j]] ]<>If[ j < len," & ",""], {j,1,len}];
        If[ i < Length[a], exp = exp<>"\\\\\n"],{i,1,Length[a] }];
        exp = exp<>"\n";
        exp = exp<>"\\end{array}\\right]";
        Return[exp];
        ];
TeX[det[a_] ] := "det \\; \\left|"<>TeX[a]<>"\\right|";
TeX[bmatrix[a_] ] := " \\left["<>TeX[a]<>"\\right]";
TeX[Conjugate[g_] ] := Typeset[ Paren[g], Sup["*"] ]
TeX[Paren[g_] ] := "\\left("<>TeX[g]<>"\\right)";
TeX[Power[g_[k_],2 ] ] := "{"<>TeX[g[k] ]<>"}"<>"^{2}";
TeX[ Power[ a_ + b_, n_] ] := "("<>TeX[ a + b]<>")^{"<>TeX[n]<>"}";

TeX[a_ + Zellipsis[3] ] := TeX[a]<>" + "<>"\\ellipsis";
TeX[plus[a_ + Zellipsis[3] ]] := TeX[plus[a]]<>" + "<>"\\ellipsis";
TeX[plus[ Zellipsis[3] ] ] := "\\ellipsis";
TeX[plus[a___ , Zellipsis[3], b___ ]] := TeX[plus[a,b]]<>" + "<>"\\ellipsis";

TeX[a_ + CC ] := "(\\,"<>TeX[a]<>" + "<>"CC\\,)";
TeX[plus[a_ + CC ]] := "(\\,"<>TeX[Plus[a]]<>" + "<>"CC\\,)";
TeX[plus[ CC ] ] := "CC";
TeX[plus[a___ , CC, b___ ]] := "(\\,"<>TeX[a+b]<>" + "<>"CC\\,)";

TeX[a_ + b__ ] := TeX[a]<>" + "<>TeX[Plus[b]];
TeX[plus[ a_ , b__ ] ] := TeX[a]<>" + "<>TeX[plus[b]];
TeX[plus[a_] ] := TeX[a];
TeX[a_ - b_] := TeX[a]<>" - "<>TeX[b];
*)
(* TeX[a_ * (s1_ + s2_) ] := TeX[a]<>"  ("<>TeX[s1 + s2 ]<>")";
*)
(*
TeX[ -1 * b_] := "-\,"<>TeX[b];
TeX[a_ *  b_] := If[ FreeQ[ a, Plus, 1 ] ,  TeX[a]<>"\\,", TeX[ Paren[a]  ]]<> If[ FreeQ[ b, Plus, 1 ] , TeX[b], TeX[ Paren[b] ]];
(* TeX[a_,b__] := TeX[a]<>TeX[b]; *)
TeX[prod[g_] h_ ] := "\\Pi_{"<>TeX[g]<>"} \; " <> TeX[h]  ;
TeX[prod[g_]] := "\\Pi_{"<>TeX[g]<>"} \; "  ;


TeX[dot[a_,b_] ] := TeX[a]<>"\\cdot "<>TeX[b];
TeX[a_ / b_] := TeX[a]<>" / "<>TeX[b];
TeX[Power[n_,1/2] ] := "\\sqrt{"<>TeX[n]<>"}";
TeX[ Power[a_,b_] ] := TeX[a]<>"^{"<>TeX[b]<>"}";
TeX[ g_ + h_.1 Power[a_,b_] ] := TeX[plus[ g, h Power[a,b] ] ];

TeX[Power[n_,-1/2] ] := "\\frac{1}{\\sqrt{"<>TeX[n]<>"}}";
TeX[Rational[a_,b_] ] := "\\frac{"<>TeX[a]<>"}{"<>TeX[b]<>"}";
TeX[sum[g_,{k__}]] := "\\Sigma_{"<>TeX[{k}]<>"}\,"<>TeX[g];
TeX[times[a_,b_] ] := TeX[a]<>"\\,"<>TeX[b];
TeX[frac[a_,b_] ] := "\\frac{"<>TeX[a]<>"}{"<>TeX[b]<>"}";
TeX[cfrac[a_,b_] ] := "\\cfrac{"<>TeX[a]<>"}{"<>TeX[b]<>"}";

TeX[dot[a_,b_] ] := TeX[a]<>"\\cdot "<>TeX[b];
TeX[ZERO] := "0";
TeX[det ] := "det \;"
TeX[a_] := ToString[ a];
*)
Typeset[a_,Sup[n___] ] := "{"<>TeX[a]<>"}^{"<>TeX[n]<>"}";
Typeset[a_,Sub[n___] ] := "{"<>TeX[a]<>"_{"<>TeX[n]<>"}}";
Typeset[a_,Sup[m___], Sub[n___] ] := "{"<>TeX[a]<>"^{"<>TeX[m]<>"}_{"<>TeX[n]<>"}}";
TeX[] := "";

Typeset[a_,b___,Sub[m_], Sup[n_],g___ ] := Typeset[a,b,Sup[n], Sub[m],g];
FileIsOpen["nonsense"] = 1;
TeX[ TeX[ g_] ] := TeX[g];

TeX[ Rule[ a_ ,  b_ ]] := TeX[ a]<>"\\rightarrow "<> TeX[b]<>"\;" ;
TeXGraphics[aa_,bb_] := Module[{},
        $PreReadSave = $PreRead;
        $PreRead = .;
        (* Export[ aa, ToExpression[FromCharacterCode[bb] ] ]; *)
        $DisplayFunctionOld=$DisplayFunction;
        Export[ aa<>".eps", bb  ];
        Export[ aa<>".pdf", bb  ];
        $DisplayFunction=System`Private`JavaDisplay;
        Show[ bb ];
        $DisplayFunction=$DisplayFunctionOld;
        $PreRead = $PreReadSave;
        Return[99];
        ];
(* TeX[ Complex[0,a_] ] := TeX[a]<>"\\,i";
TeX[ Complex[a_,b_] ] := "("<>TeX[a]<>"+"<>TeX[b]<>"\\,i)";
TeX[ (-1)^s ] := "(-1)^s";
*)
TeXDependence = Identity;
TeXStrip[name_ ] := TeXStrip[name<>".mtex",name<>".m"];
TeXStrip[infile_, outfile_] := Module[{str,stretest,left,right,leftchoice,rightchoice,nmax,level,st,st0,st1},
        str = BinaryReadList[infile ];
        strtest = ToCharacterCode[ "asdasdf(*[  strip this ]*) not this (** and this **)"];
        left = ToCharacterCode["(*" ];
        right = ToCharacterCode["*)" ];
        leftchoice = (ToCharacterCode["$"][[1]]  | ToCharacterCode["*"][[1]] | ToCharacterCode["["][[1]]);
        rightchoice = (ToCharacterCode["$"][[1]] | ToCharacterCode["*"][[1]] | ToCharacterCode["]"][[1]]);
        nmax = Length[ str ];
        level = 0;
	leftpos[level] = 0;
	rightpos[level] = 0;
	levelmax = 0;
	levelmin = 0;
	lineno = 0;
	errorline = 0;
        st = {};
	newline = ToCharacterCode["\n"][[1]];
        Do[ levelmin = Min[ level, levelmin]; levelmax = Max[ level, levelmax ];
		 If[ str[[i]]  ==  newline , lineno++];
		 If[ ( i+2 <=  nmax )  &&  { str[[i]], str[[i+1]] } == left && MatchQ[str[[i+2]], leftchoice] ,  leftpos[++level] = lineno ];
                If[ ( i-3 >= 1 )      && {  str[[i-2]], str[[i-1]] } == right && MatchQ[str[[i-3]], rightchoice] ,  rightpos[level--] = lineno ;
			If[ level < 0, (* Print[ "level = -1 on line ", lineno ] ;  *)
			 If[ errorline == 0, errorline =  lineno ] ] ];
                If[ level == 0, st = Join[st,{str[[i]] } ]], {i,1,nmax}];
        st0 = FromCharacterCode[ st ];
	(*[*)
        st1 = StringReplace[st0,{ RegularExpression["\\(\\(\\(\\(\\s*"] -> "",
                          RegularExpression["\\s*\\)\\)\\)\\)(\\;| )*"] -> "",
                          RegularExpression["\n\n+"] -> "\n\n",
                          RegularExpression[".*TeX\[.*\].*\n"] -> "" ,
                          RegularExpression["TeXGraphics.\".*\" *\,"] -> "Show[" , (* ] *)
                          RegularExpression["Needs\[.*TeX.*\].*\n"] -> "",
                          RegularExpression["TeXDependence\[.*\].*\n"] -> "",
                          RegularExpression["TeXStrip\[.*\].*\n"] -> ""} ];
        pos = StringPosition[st1, RegularExpression[" *\\<\\<.*\\.mtex *\\; *"] ];
        subs = StringTake[ st1, pos ];
        filn = StringReplace[subs,{RegularExpression["^ *\\<\\< *"] :> "",
                          RegularExpression["\\.mtex *\\;.*"] :> "" }];
        newst = st1;
        new = Table[ "Needs[\""<>filn[[i]]<>"`\"];",{i,1,Length[pos] }];
        newst = StringReplacePart[st1 , new,pos ];
        BinaryWrite[outfile, st1 ];
	Close[ outfile];
	(* Print["total number of lines = ", lineno, "final level = ", level ,"\n"];
	Print["lines of last level  = {", {leftpos[level ], rightpos[level ] },"}\n" ] ;
	*)
	$errormsg = ToString[lineno]<>" lines no syntax errors ";
	If[ level > 0 , $errormsg = "no matching paren for line "<>ToString[ leftpos[level] + 1 ] ] ;
	If[ level < 0 , $errormsg = "premature closing paren on line "<>ToString[ errorline ] ] ;
	(* Do[ Print[" level = ", l , " lines = {", {leftpos[l ], rightpos[l ] },"}\n" ] ,
		{l,levelmin,levelmax}];
	*)
	Print[$errormsg];
        Return[ $errormsg ]
        ];

FigInsert[filnn_,w_,cap_] := Module[{},
	If[ ValueQ[ CURRENTFile  ],
	filn = StringReplace[ filnn, { RegularExpression["(.jpg|.pdf|.jpeg|.eps)"] -> ""  }];
	$doc = OpenAppend[CURRENTFile];
	outstring = "\\renewcommand{\\filename}{}
		\\begin{figure}[h] 
		\\includegraphics[width="<>ToString[w]<>"cm]{"<>filn<>"}
		\\caption{"<>cap<>"}
		\\label{fig:"<>filn<>"}
		\\end{figure}\n";
	WriteString[$doc, outstring];
        Close[$doc] ];
	];

FigSave[filnn_,gr_] := Module[{},
        oldpre = $PreRead;
        $PreRead = .;
        If[ $BatchInput == True,
                $DisplayFunction = System`Private`JavaDisplay ];
        Show[gr];
        filn = StringReplace[ filnn, { RegularExpression["(.jpg|.pdf|.jpeg|.eps)"] -> ""  }];
        Export[filn<>".eps",gr];
        Export[filn<>".pdf",gr];
        Export[filn<>".jpg",gr];
        If[ $BatchInput == True,
                $DisplayFunction = Identity ];
        $PreRead = oldpre;
	filn = StringReplace[filn, {RegularExpression[".*/"] -> "" }];
	Print["filn=", filn ];
        Return[filn]
        ];

StringJoinCommaSeparated[x_] := Drop[ Flatten[Table[ { x[[i]], ","}, {i,1,Length[x]}],1],-1];
(* TeXFormat[f_[x___]] := ToString[f]<>"["<>StringJoinCommaSeparated[ ToString /@ {x} ]<>"]";
TeXFormat[ yy_]  := ToString[yy]; 
*)
TeXFunc[g_] := ToString[g];
TeXFormat[g_] := ToString[g];
texdef[g_] := ToString[TeXFormat[ ToExpression[StringReplace[ g, "\\" -> "" ]] ]];

TeX[tex_] := FixedPoint[ detext, ToString[ TeXForm[ tex] ] ];
detext[tex_] := Module[{},
	str = StringReplace[ tex, RegularExpression["\\\\text{([^}]*)}$"] :> TeXFunc[texdef["$1"]]  ];
	str = StringReplace[ str, RegularExpression["\\\\text{([^}]*)}([^(])"] :> TeXFunc[texdef["$1"]<>"$2"]  ];
	str = StringReplace[ str, RegularExpression["\\\\text{([^}]*)}[(]([^)]*)[)]"] :> TeXFunc[texdef["$1[$2]"]] ];  
	Return[ str ]
	];


SetAttributes[$PreRead, HoldAllComplete ];  
If[ $BatchInput == True, (* Line added 2008-06-28 *)
	$PreRead = ( ReInterpret["\<"<>ToString[#]<>"\>"]; # ) &;
	];
End[];
EndPackage[];
