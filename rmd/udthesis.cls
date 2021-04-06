%% 
%% This is file `udthesis.cls', generated on <2020/10/13>
%% Depends on report.cls, hyperref and hypcap package.
%% 
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{udthesis}[2008/7/01 Standard UDThesis LaTeX class]
\DeclareOption{titlepage}%
    {\ClassError{udthesis}{Option `titlepage' not supported}{}}
\DeclareOption{seniorthesis}%
    {\AtEndOfClass{\input{seniorthesis.clo}}}
\PassOptionsToClass{12pt}{report}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\ProcessOptions
\LoadClass{report}

% Automatically load hyperref for rich PDF
% Added breaklinks to allow hypenation and breaking for long citations

\AtEndOfClass{%
        \usepackage[bookmarksnumbered,linktocpage,colorlinks,%
        linkcolor=blue,citecolor=blue,urlcolor=blue,breaklinks]{hyperref}%
        \def\@pdfcreator{LaTeX2e, UDThesis class, hyperref package}%
        \usepackage[all]{hypcap}}
\AtBeginDocument{%
        \hypersetup{pdftitle=\@pdftitle,pdfauthor=\@pdfauthor,%
        pdfkeywords=\@pdfkeywords,pdfsubject=\@pdfsubject}}
            
\newcommand{\@pdftitle}{}
\newcommand{\@pdfauthor}{}
\newcommand{\@pdfkeywords}{}
\newcommand{\@pdfsubject}{}

\renewcommand*{\title}[2][\@empty]{%
    \gdef\@title{#2}\ifx\@empty#1%
        \gdef\@pdftitle{#2}%
    \else\gdef\@pdftitle{#1}\fi}
       
\renewcommand*{\author}[2][\@empty]{%
    \gdef\@author{#2}\ifx\@empty#1%
        \gdef\@pdfauthor{#2}%
    \else\gdef\@pdfauthor{#1}\fi}
      
\newcommand*{\keywords}[1]{%
    \gdef\@pdfkeywords{#1}}
    
\newcommand*{\subject}[1]{%
    \gdef\@pdfsubject{#1}}
    
% Make page number wider in TOC for large page numbers beyond 3 digits for example xviii

\renewcommand{\@pnumwidth}{2.2em}

\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}

% Single space certain environments

\renewenvironment{enumerate}{%
  \ifnum \@enumdepth >\thr@@\@toodeep\else
    \advance\@enumdepth\@ne
    \edef\@enumctr{enum\romannumeral\the\@enumdepth}%
      \expandafter
      \list
        \csname label\@enumctr\endcsname
        {\renewcommand{\baselinestretch}{1}\large\normalsize
        \usecounter\@enumctr\def\makelabel##1{\hss\llap{##1}}}%
  \fi}{\endlist}
  
\renewenvironment{itemize}{%
  \ifnum \@itemdepth >\thr@@\@toodeep\else
    \advance\@itemdepth\@ne
    \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
    \expandafter
    \list
      \csname\@itemitem\endcsname
      {\renewcommand{\baselinestretch}{1}\large\normalsize
      \def\makelabel##1{\hss\llap{##1}}}%
  \fi}{\endlist}
  
\renewenvironment{quotation}
               {\list{}{\listparindent 1.5em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \renewcommand{\baselinestretch}{1}\large\normalsize
                \item[]}
               {\endlist}

\renewenvironment{quote}
               {\list{}{\rightmargin\leftmargin}%
                \renewcommand{\baselinestretch}{1}\large\normalsize
                \item[]}
               {\endlist}

\renewenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                \renewcommand{\baselinestretch}{1}\large\normalsize
                \let\makelabel\descriptionlabel}}
               {\endlist}

\renewcommand{\descriptionlabel}[1]{\hspace\labelsep
                                \normalfont\bfseries #1}

% Additional description list for glossary

\newenvironment{desc}
               {\list{}{\labelwidth 1in 
                \advance\leftmargin by \labelwidth
                \renewcommand{\baselinestretch}{1}\large\normalsize
                \let\makelabel\desclabel}}
               {\endlist}
               
\newcommand{\desclabel}[1]{\normalfont\bfseries #1\hss}

% Section command changes

\renewcommand{\@afterheading}{\global\@nobreaktrue
    \everypar{\if@nobreak
                 \global\@nobreakfalse
                 \clubpenalty \@M
              \else \clubpenalty \@clubpenalty
                 \everypar{}\fi}}

\newcommand*{\@makenchapterhead}[1]{\vspace*{.6in} {\parindent 0pt 
 \raggedright 
 \ifnum \c@secnumdepth >\m@ne \begin{center}\renewcommand{\baselinestretch}{1}\large\normalsize\bfseries \@chapapp{} 
 \thechapter \\ 
 \vskip 10pt \fi \bfseries \uppercase{#1}\end{center}
 \nobreak \vskip 15pt } }

\newcommand*{\@makenochapterhead}[1]{\vspace*{.6in} {\parindent 0pt 
 \raggedright
 \ifnum \c@secnumdepth >\m@ne \begin{center}\renewcommand{\baselinestretch}{1}\large\normalsize\bfseries \@chapapp{} \\
 \vskip 10pt \fi \bfseries \uppercase{#1}\end{center}
 \nobreak \vskip 15pt } }
 
\newcommand*{\@makenschapterhead}[1]{\vspace*{.6in} {\parindent 0pt 
 \raggedright
 \begin{center}\renewcommand{\baselinestretch}{1}\large\normalsize\bfseries \uppercase{#1}\par \end{center}
 \nobreak \vskip 15pt } }

\renewcommand*{\@makeschapterhead}[1]{\vspace*{.6in} {\parindent 0pt 
 \raggedright
 \begin{center}\renewcommand{\baselinestretch}{1}\large\normalsize\bfseries \uppercase{#1}\par \end{center}
 \nobreak \vskip 15pt } }

\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
 \refstepcounter{chapter}
 \typeout{\@chapapp\space\thechapter.}
 \addcontentsline{toc}{chapter}{\protect
 \numberline{\thechapter}#1}\else
 \addcontentsline{toc}{chapter}{#1}\fi
 \chaptermark{#1}
\if@twocolumn \@topnewpage[\@makenchapterhead{#2}]
 \else \@makenchapterhead{#2}
 \@afterheading \fi}
 
\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\partname\ \protect\numberline{\thepart}\protect\uppercase{#1}}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    \markboth{}{}%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >-2\relax
       \bfseries \partname\nobreakspace\thepart
       \par
       \vskip 20\p@
     \fi
     \bfseries \uppercase{#2}\par}%
    \@endpart}
    
\renewcommand{\appendix}{\if\app0\def\@chapapp{Appendix}
\setcounter{chapter}{0}
\def\thechapter{\Alph{chapter}}\gdef\app{1}\fi
\setcounter{section}{0}
\setcounter{equation}{0}
\setcounter{figure}{0}
\setcounter{table}{0}
\clearpage \thispagestyle{plain} \global\@topnum\z@
\@afterindenttrue \secdef\@appendix\@sappendix}

\def\@appendix[#1]#2{\ifnum \c@secnumdepth >\m@ne
 \refstepcounter{chapter}
 \typeout{\@chapapp\space\thechapter.}
 \phantomsection
 \addcontentsline{toc}{appendix}{\protect\numberline{\thechapter}#1}
 \else\addcontentsline{toc}{chapter}{#1}\fi
 \chaptermark{#1}
\if@twocolumn \@topnewpage[\@makenchapterhead{#2}]
 \else \@makenchapterhead{#2}
 \@afterheading \fi}

\def\@sappendix#1{\if@twocolumn \@topnewpage[\@makenschapterhead{#1}]
 \else \@makenschapterhead{#1}
 \@afterheading\fi}

\def\oneappendix{\setcounter{chapter}{0}
\setcounter{section}{0}
\setcounter{equation}{0}
\setcounter{figure}{0}
\setcounter{table}{0}
\def\@chapapp{Appendix}
\def\thechapter{\Alph{chapter}}
\clearpage \thispagestyle{plain} \global\@topnum\z@
\@afterindenttrue \secdef\@oneappendix\@soneappendix}

\def\@oneappendix[#1]#2{\ifnum \c@secnumdepth >\m@ne
 \refstepcounter{chapter}
 \typeout{\@chapapp.}
 \phantomsection
 \addcontentsline{toc}{appendix}{\protect\numberline{}#1}\fi
 \chaptermark{#1}
\if@twocolumn \@topnewpage[\@makenochapterhead{#2}]
 \else \@makenochapterhead{#2}
 \@afterheading \fi}
 
\def\@soneappendix#1{\if@twocolumn \@topnewpage[\@makenschapterhead{#1}]
\else \@makenschapterhead{#1}
 \@afterheading\fi}

\renewcommand{\section}{\@startsection {section}{1}{\z@}%
                                   {3ex \@plus 1ex \@minus .2ex}%
                                   {1pt \@plus.2ex}%
                                   {\reset@font\normalsize\bfseries}}
\renewcommand{\subsection}{\@startsection{subsection}{2}{\z@}%
                                    {3ex\@plus 1ex \@minus .2ex}%
                                    {1pt \@plus .2ex}%
                                    {\reset@font\normalsize\bfseries}}
\renewcommand{\subsubsection}{\@startsection{subsubsection}{3}{\z@}%
                                    {3ex\@plus 1ex \@minus .2ex}%
                                    {1pt \@plus .2ex}%
                                    {\reset@font\normalsize\bfseries}}
\renewcommand{\paragraph}{\@startsection{paragraph}{4}{\z@}%
                                    {3ex \@plus1ex \@minus .2ex}%
                                    {1pt}%
                                    {\reset@font\normalsize\bfseries}}
\renewcommand{\subparagraph}{\@startsection{subparagraph}{5}{\z@}%
                                      {3ex \@plus1ex \@minus .2ex}%
                                      {1pt}%
                                      {\reset@font\normalsize\bfseries}}

% Table of Contents changes

\renewcommand{\tableofcontents}{%
 \@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
 \fi\phantomsection%
 \pdfbookmark[0]{Table of Contents}{chapter}%
 \chapter*{Table of Contents\@mkboth{CONTENTS}{CONTENTS}}
 \vspace*{\baselineskip}
 {\let\linebreak=\space\parskip=0pt\pretolerance=10000\raggedright
 \advance\hsize by -\@pnumwidth
 \renewcommand{\baselinestretch}{1}\large\normalsize
 \@starttoc{toc}\if@restonecol\twocolumn\fi}}

\renewcommand{\@dottedtocline}[5]{\ifnum #1>\c@tocdepth \else
  \vskip \z@ plus .2pt
  {\leftskip #2\relax \parfillskip -\@pnumwidth
    \parindent #2\relax\@afterindenttrue
   \interlinepenalty\@M
   \leavevmode
   \@tempdima #3\relax \advance\leftskip \@tempdima 
   \hbox{}\hskip -\leftskip
    #4\nobreak\leaders\hbox{$\m@th \mkern \@dotsep mu.\mkern \@dotsep
       mu$}\hfill \nobreak \hbox to\@pnumwidth{\hfil\rm #5}\par}\fi}

\renewcommand*{\l@part}[2]{\pagebreak[3]
 \@tempdima 1.5em
 \let\previouslevel=\currentlevel
 \let\currentlevel=0
 \vspace*{\baselineskip}
 \begingroup
 \parindent \z@
 \parfillskip -\@pnumwidth
 %\hangindent\@tempdima
 \hangindent 4.1em
 \leavevmode
 {\bfseries #1}\nobreak\leaders\hbox{$\m@th \mkern \@dotsep
mu.\mkern
 \@dotsep mu$}\hfill \nobreak \hbox to\@pnumwidth{\bfseries\hfil #2}\par
 \endgroup}

\renewcommand*{\l@chapter}[2]{\pagebreak[3]
 \@tempdima 1.5em
 \let\previouslevel=\currentlevel
 \let\currentlevel=0
 \vspace*{\baselineskip}
 \begingroup
 \parindent \z@
 \parfillskip -\@pnumwidth
 \if\firstchap0{\bfseries Chapter}\\
 \vspace*{\baselineskip}\gdef\firstchap{1}%
    \else\relax\fi
 \hangindent\@tempdima
 \leavevmode
 {\uppercase{\bfseries #1}}\nobreak\leaders\hbox{$\m@th \mkern \@dotsep
mu.\mkern
 \@dotsep mu$}\hfill \nobreak \hbox to\@pnumwidth{\bfseries\hfil #2}\par
 \endgroup}

\newcommand*{\l@appendix}[2]{\pagebreak[3]
 \@tempdima 1.5em
 \let\previouslevel=\currentlevel
 \let\currentlevel=6
 \if\currentlevel\previouslevel\relax\else\vspace*{\baselineskip}\fi
 \begingroup
 \parindent \z@
 \parfillskip -\@pnumwidth
 \if\firstapp0{\bfseries Appendix}\\
 \vspace*{\baselineskip}\gdef\firstapp{1}%
    \else\relax\fi
 \hangindent\@tempdima
 \leavevmode
 {\uppercase{\bfseries #1}}\nobreak\leaders\hbox{$\m@th \mkern \@dotsep
mu.\mkern
 \@dotsep mu$}\hfill \nobreak \hbox to\@pnumwidth{\bfseries\hfil #2}\par
 \endgroup}

\newcommand*{\l@oneappendix}[2]{\pagebreak[3]
 \@tempdima 1.5em
 \let\previouslevel=\currentlevel
 \let\currentlevel=6
 \if\currentlevel\previouslevel\relax\else\vspace*{\baselineskip}\fi
 \begingroup
 \parindent \z@
 \parfillskip -\@pnumwidth
 {\bfseries Appendix}\\
 \vspace*{\baselineskip}
 \leavevmode
 {\uppercase{\bfseries #1}}\nobreak\leaders\hbox{$\m@th \mkern \@dotsep
mu.\mkern
 \@dotsep mu$}\hfill \nobreak \hbox to\@pnumwidth{\bfseries\hfil #2}\par
 \endgroup}

\newcommand{\l@prefacesection}[2]{\pagebreak[3]
 \@tempdima 1.5em
 \let\previouslevel=\currentlevel
 \let\currentlevel=7
 \if\currentlevel\previouslevel\relax\else\vspace*{\baselineskip}\fi
 \begingroup
 \parindent \z@
 \parfillskip -\@pnumwidth
 \hangindent\@tempdima
 \leavevmode
 {\uppercase{\bfseries #1}}\nobreak\leaders\hbox{$\m@th \mkern \@dotsep
mu.\mkern
 \@dotsep mu$}\hfill \nobreak \hbox to\@pnumwidth{\bfseries\hfil #2}\par
 \endgroup}

\renewcommand*{\l@section}{\let\previouslevel=\currentlevel
               \let\currentlevel=1
               \if\currentlevel\previouslevel\relax%
                  \else\vspace*{\baselineskip}\fi
               \@dottedtocline{1}{1.5em}{2.3em}}
\renewcommand*{\l@subsection}{\let\previouslevel=\currentlevel
                  \let\currentlevel=2
                  \if\currentlevel\previouslevel\relax%
                     \else\vspace*{\baselineskip}\fi
                  \@dottedtocline{2}{3.8em}{3.2em}}
\renewcommand*{\l@subsubsection}{\let\previouslevel=\currentlevel
                     \let\currentlevel=3
                     \if\currentlevel\previouslevel\relax%
                        \else\vspace*{\baselineskip}\fi
                     \@dottedtocline{3}{7.0em}{4.1em}}
\renewcommand*{\l@paragraph}{\let\previouslevel=\currentlevel
                 \let\currentlevel=4
                 \if\currentlevel\previouslevel\relax%
                    \else\vspace*{\baselineskip}\fi
                 \@dottedtocline{4}{11.1em}{5em}}
\renewcommand*{\l@subparagraph}{\let\previouslevel=\currentlevel
                    \let\currentlevel=5
                    \if\currentlevel\previouslevel\relax%
                       \else\vspace*{\baselineskip}\fi
                    \@dottedtocline{5}{11.1em}{5em}}

% List of Figure/Table changes

\renewcommand{\listoffigures}{%
 \@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
 \fi\chapter*{List of Figures\@mkboth{LIST OF FIGURES}{LIST OF FIGURES}}
\addcontentsline{toc}{prefacesection}{List of Figures}
 \vspace*{\parskip}
 {\pretolerance=10000\raggedright
 \advance\hsize by -\@pnumwidth
 \renewcommand{\baselinestretch}{1}\large\normalsize
 \parskip\baselineskip
 \@starttoc{lof}\if@restonecol\twocolumn\fi}}

% Increased 3em to 3.2em to handle double digit chapter/figtab numbers
% Redefined \l@table to be the new \l@figure - 1/25/99
\renewcommand*{\l@figure}{\@dottedtocline{1}{1.5em}{3.2em}}
\let\l@table\l@figure

\renewcommand{\listoftables}{%
 \@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
 \fi\chapter*{List of Tables\@mkboth{LIST OF TABLES}{LIST OF TABLES}}
\addcontentsline{toc}{prefacesection}{List of Tables}
 \vspace*{\parskip}
 {\pretolerance=10000\raggedright
 \advance\hsize by -\@pnumwidth
 \renewcommand{\baselinestretch}{1}\large\normalsize
 \parskip\baselineskip
 \@starttoc{lot}\if@restonecol\twocolumn\fi}}

% New version of xfloat so hyperref package will work

\def\@xfloat #1[#2]{%
  \@nodocument
  \def \@captype {#1}%
   \def \@fps {#2}%
   \@onelevel@sanitize \@fps
   \def \reserved@b {!}%
   \ifx \reserved@b \@fps
     \@fpsadddefault
   \else
     \ifx \@fps \@empty
       \@fpsadddefault
     \fi
   \fi
   \ifhmode
     \@bsphack
     \@floatpenalty -\@Mii
   \else
     \@floatpenalty-\@Miii
   \fi
  \ifinner
     \@parmoderr\@floatpenalty\z@
  \else
    \@next\@currbox\@freelist
      {%
       \@tempcnta \sixt@@n
       \expandafter \@tfor \expandafter \reserved@a
         \expandafter :\expandafter =\@fps
         \do
          {%
           \if \reserved@a h%
             \ifodd \@tempcnta
             \else
               \advance \@tempcnta \@ne
             \fi
           \fi
           \if \reserved@a t%
             \@setfpsbit \tw@
           \fi
           \if \reserved@a b%
             \@setfpsbit 4%
           \fi
           \if \reserved@a p%
             \@setfpsbit 8%
           \fi
           \if \reserved@a !%
             \ifnum \@tempcnta>15
               \advance\@tempcnta -\sixt@@n\relax
             \fi
           \fi
           }%
       \@tempcntb \csname ftype@\@captype \endcsname
       \multiply \@tempcntb \@xxxii
       \advance \@tempcnta \@tempcntb
       \global \count\@currbox \@tempcnta
       }%
    \@fltovf
  \fi
  \global \setbox\@currbox
    \color@vbox
      \normalcolor
      \vbox \bgroup
	\renewcommand{\baselinestretch}{1}\large\normalsize
        \hsize\columnwidth
        \@parboxrestore
        \@floatboxreset
}

% Bibliography changes -- updated to reflect removal of \if@openbib

\renewenvironment{thebibliography}[1]
     {\newcommand{\upperbibname}{\uppercase\expandafter{\bibname}}
      \chapter*{\upperbibname
        \@mkboth{\uppercase{\bibname}}{\uppercase{\bibname}}}%
      \typeout{\bibname.}%
      \addcontentsline{toc}{chapter}{\upperbibname}
      \renewcommand{\baselinestretch}{1}\large\normalsize
      \list{\@biblabel{\arabic{enumiv}}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand{\theenumiv}{\arabic{enumiv}}}%
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

\newenvironment{thereferences}
     {\newcommand{\upperbibname}{\uppercase\expandafter{\bibname}}
      \chapter*{\upperbibname
        \@mkboth{\uppercase{\bibname}}{\uppercase{\bibname}}}%
      \typeout{\bibname.}%
      \addcontentsline{toc}{chapter}{\upperbibname}
      \begingroup
      \clubpenalty=10000
      \widowpenalty=10000
      \def\baselinestretch{1}\large\normalsize\parskip\baselineskip
      \parindent 0pt\sfcode`\.=\@m
      \everypar{\hangindent3em}}%
     {\par\endgroup}

% Miscellaneous changes

\@addtoreset{footnote}{part}
\long\def\@makefntext#1{%
\noindent $^{\@thefnmark}$\enspace#1}

\long\def\@footnotetext#1{\insert\footins{\renewcommand{\baselinestretch}{1}
    \large\normalsize
    \interlinepenalty\interfootnotelinepenalty
    \splittopskip\footnotesep
    \splitmaxdepth \dp\strutbox \floatingpenalty \@MM
    \hsize\columnwidth \@parboxrestore
   \edef\@currentlabel{\csname
    p@footnote\endcsname\@thefnmark}\@makefntext
    {\rule{\z@}{\footnotesep}\ignorespaces
      #1\strut}}}

\newcommand{\captioncont}{%
   \ifx\@captype\@undefined
     \@latex@error{\noexpand\caption outside float}\@ehd
     \expandafter\@gobble
   \else
     %\refstepcounter\@captype
     \expandafter\@firstofone
   \fi
	\@dblarg{\@captioncont\@captype}}
 
\long\def\@captioncont#1[#2]#3{\par\begingroup
    \@parboxrestore
    \normalsize
    \@makecaption{\csname fnum@#1\endcsname}{\ignorespaces #3}\par
  \endgroup}
 
\long\def\@caption#1[#2]#3{\par\addcontentsline{\csname
  ext@#1\endcsname}{#1}{\protect\numberline{\csname
  the#1\endcsname}{\ignorespaces #2}}\begingroup
    \@parboxrestore
    \normalsize
    \@makecaption{\csname fnum@#1\endcsname}{\ignorespaces #3}\par
  \endgroup}

\long\def\@makecaption#1#2{\vspace*{1.5\baselineskip}
 \setbox0=\hbox{{\bfseries #1:\enspace}}
 \setbox\@tempboxa\hbox{{\bfseries #1:\enspace}#2}
 \ifdim \wd\@tempboxa >\hsize 
        \hangindent\wd0\unhcopy0#2\par 
 \else  \hbox to\hsize{\hfil\box\@tempboxa\hfil}\vspace*{\baselineskip}
 \fi}

\def\@verbatim{\trivlist \item[]\if@minipage\else\vskip\parskip\fi
\renewcommand{\baselinestretch}{1}
\leftskip\@totalleftmargin\rightskip\z@
\parindent\z@\parfillskip\@flushglue\parskip\z@
\@tempswafalse \def\par{\if@tempswa\hbox{}\fi\@tempswatrue\@@par}
\obeylines \tt \catcode``=13 \@noligs \let\do\@makeother \dospecials}

%
%Additions and Modifications for UDThesis
%
\setlength{\oddsidemargin}{.44in} 
\setlength{\evensidemargin}{0in}
\setlength{\topmargin}{-.24in} 
\setlength{\headsep}{0pt} 
\setlength{\parindent}{3em}
\setlength{\textheight}{8.65in}
\setlength{\textwidth}{6.1in}
\setlength{\footskip}{.75in}
\brokenpenalty=10000
\clubpenalty4000\widowpenalty4000\displaywidowpenalty4000
% all entries will appear in TOC except subparagraph
\setcounter{tocdepth}{4}
\setcounter{secnumdepth}{4}
\renewcommand{\@chapapp}{Chapter}
\renewcommand{\ps@headings}{\pagestyle{plain}}
\renewcommand{\baselinestretch}{1.5}
\let\currentlevel=0
\let\previouslevel=0
\gdef\firstchap{0}
\gdef\firstapp{0}
\gdef\app{0}
\footnotesep=1.25\baselineskip
%\advance\footnotesep by \@ptsize pt
\interfootnotelinepenalty 200


%
% Title and Approval Page
%
\newcommand{\department}[1]{\gdef\@department{#1}}
\newcommand{\degreedate}[1]{\gdef\@degreedate{#1}\extractyear{#1}}
\newcommand{\type}[1]{\gdef\@type{#1}}
\newcommand{\degree}[1]{\gdef\@degree{#1}}
\newcommand{\majorfield}[1]{\gdef\@majorfield{#1}}
\newcommand{\copyrightyear}[1]{\gdef\@copyrightyear{#1}} 
           % \author, \title in report
\renewcommand{\@title}{}\renewcommand{\@author}{}
\newcommand{\@type}{}\newcommand{\@majorfield}{}\newcommand{\@degree}{}

\newif\iffigurespage \newif\iftablespage \newif\ifsignedpage 
\newif\ifsignedpagecont \newif\ifmajorfield
\figurespagetrue \tablespagetrue \signedpagefalse \signedpagecontfalse
\majorfieldfalse

%
% Legal Department suggested to the Graduate Office to have
% all theses and dissertations copyrighted.
%
% Modified to handle dates with zeros 
\gdef\firnum{0} \gdef\secnum{0} \gdef\thinum{0} \gdef\founum{0} 
\gdef\setfirnum{0} \gdef\setsecnum{0} \gdef\setthinum{0} \gdef\setfounum{0} 
\gdef\anumber{0}
\let\tstnumzero=0
\let\tstnumone=1 \let\tstnumtwo=2 \let\tstnumthree=3 \let\tstnumfour=4
\let\tstnumfive=5 \let\tstnumsix=6 \let\tstnumseven=7 \let\tstnumeight=8
\let\tstnumnine=9

\newcommand{\checknumber}[1]{\if#1\tstnumzero\gdef\anumber{1}\fi
\if#1\tstnumone\gdef\anumber{1}\fi
\if#1\tstnumtwo\gdef\anumber{1}\fi
\if#1\tstnumthree\gdef\anumber{1}\fi
\if#1\tstnumfour\gdef\anumber{1}\fi
\if#1\tstnumfive\gdef\anumber{1}\fi
\if#1\tstnumsix\gdef\anumber{1}\fi
\if#1\tstnumseven\gdef\anumber{1}\fi
\if#1\tstnumeight\gdef\anumber{1}\fi
\if#1\tstnumnine\gdef\anumber{1}\fi}

\newcommand{\extractyear}[1]{{\getyear#1\end 
    \gdef\@copyrightyear{\firnum\secnum\thinum\founum}}}
\newcommand{\getyear}[1]{\ifx#1\end \let\next=\relax \else 
               \checknumber{#1}\if\anumber1\if\setfirnum0\gdef\firnum{#1}\gdef\setfirnum{1}\else
               \if\setsecnum0\gdef\secnum{#1}\gdef\setsecnum{1}\else
               \if\setthinum0\gdef\thinum{#1}\gdef\setthinum{1}\else
               \if\setfounum0\gdef\founum{#1}\gdef\setfounum{1}\else\gdef\anumber{0}\fi
               \fi\fi\fi\fi
               \let\next=\getyear \fi \next}

\newskip\titlebaselineskip
	\titlebaselineskip=25pt

\newcommand{\approval}[1]{\vskip .5in plus .5fil minus .25in
    \setbox0=\hbox to \hsize{Approved:\enskip\hrulefill}
	\box 0 #1\par}
\newcommand{\signed}[1]{\vskip .5in
    {\list{}{\leftmargin\parindent \topsep\z@ \partopsep\z@ \itemsep\z@}
    \item[] I certify that I have read
	this \@type\ and that in my opinion 
	it meets the academic and professional 
    standard required by the University as 
    a \@type\ for the degree of
	\@degree.\endlist}
	\vskip .5in
    \setbox0=\hbox to \hsize{Signed:\enskip\hrulefill} 
	\box 0 #1\par}
\newcommand{\prof}[1]{\renewcommand{\baselinestretch}{1}\large\normalsize
	\approval{#1}
	Professor in charge of \@type\ on behalf of the Advisory Committee\par}
\newcommand{\profmember}[1]{\renewcommand{\baselinestretch}{1}\large\normalsize
	\signed{#1}
	Professor in charge of \@type\par}
\newcommand{\member}[1]{\renewcommand{\baselinestretch}{1}\large\normalsize
	\signed{#1}
	Member of \@type\ committee\par}
\newcommand{\chair}[2]{\renewcommand{\baselinestretch}{1}\large\normalsize
			   \approval{#1}
			   \hangindent\wd 1
	           #2\par} 
\newcommand{\auxchair}[2]{\renewcommand{\baselinestretch}{1}\large\normalsize
				  \approval{#1}
			      \hangindent\wd 1
	              #2\par} 
\newcommand{\dean}[2]{\renewcommand{\baselinestretch}{1}\large\normalsize
			  \approval{#1}
			  \hangindent\wd 1
	          #2\par} 
\newcommand{\provost}{\renewcommand{\baselinestretch}{1}\large\normalsize
	\approval{Louis F. Rossi, Ph.D.}
    \hangindent\wd 1 Vice Provost for Graduate and Professional Education and\newline Dean of the Graduate College\par}
\newcommand{\maketitlebox}{\vbox{\begin{center}\baselineskip\titlebaselineskip
				  {\bfseries\uppercase\expandafter{\@title}}\end{center}}}
\newcommand{\makeauthorbox}{\vbox{\begin{center}\baselineskip\titlebaselineskip
                   {by\par\@author\par}\end{center}}}
\newcommand{\maketitlepage}{\newpage\thispagestyle{empty}
	  \begingroup \parskip 0pt
    \vglue .75in plus 1.2fil minus .5in
	  \maketitlebox
    \vskip .75in plus 1.8fil minus .25in
	  \makeauthorbox
	  \vskip .75in plus 2fil minus .5in

    \renewcommand{\baselinestretch}{1}\large\normalsize
    \pretolerance=10000
		A \@type\ submitted to the 
		Faculty of the University of Delaware in partial fulfillment 
		of the requirements for the degree of 
		\@degree\ifmajorfield\  in \@majorfield\fi\par
    
    \vskip .4in plus 2fil minus .5in

    \centerline{\@degreedate}
%
% Copyright use to be optional (\copyrighttrue/false no longer valid)
%
	  \vskip .4in plus 2fil minus .5in

		\begin{center}
        \renewcommand{\baselinestretch}{1}\large\normalsize
				\copyright\ \@copyrightyear\ \@author\\%
			   All Rights Reserved\\%
		\end{center}%
    \endgroup}

\newenvironment{approvalpage}{\newpage\thispagestyle{empty}
	\begingroup\parskip 0pt
    \setbox1=\hbox{Approved:\enskip}
    \vglue 1.0in plus 1.2fil minus .5in
    \maketitlebox	
	\vskip .5in plus 1.5fil minus .25in % FIX
	\makeauthorbox
	\parindent\wd 1
	\vskip .25in plus .5fil minus .25in}%
    {\provost\endgroup}

\newenvironment{signedpage}{\newpage\thispagestyle{empty}
	\begingroup\parskip 0pt
    \setbox1=\hbox{Signed:\enskip}
	\parindent\wd 1}%
    {\endgroup\global\signedpagetrue}
% 12/11/97 added \global to properly set \signedpagetrue 

\newenvironment{signedpagecont}{\newpage\thispagestyle{empty}
	\begingroup\parskip 0pt
    \setbox1=\hbox{Signed:\enskip}
	\parindent\wd 1}%
    {\endgroup\global\signedpageconttrue}

\newenvironment{front}{\newpage
	\pagenumbering{roman}
	\ifsignedpage\setcounter{page}{4}\else\setcounter{page}{3}\fi
	\ifsignedpagecont\setcounter{page}{5}\fi}%
    {\newpage\pagenumbering{arabic}}

\newcommand{\prefacesection}[1]{
	\chapter*{#1}}

\newcommand{\prefacesectiontoc}[1]{
	\chapter*{#1}\addcontentsline{toc}{prefacesection}{#1}}

\newcommand{\maketocloflot}{\newpage
	\tableofcontents
	\iftablespage
		\listoftables
	\fi
	\iffigurespage
		\listoffigures
	\fi}
