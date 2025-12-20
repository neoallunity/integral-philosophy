% journal.cls — минимальный класс журнала
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{journal}[2025/12/15 Journal class v0.1]

\LoadClass[11pt,a4paper,openany]{book}

% Базовые пакеты
\RequirePackage{ifthen}
\RequirePackage{graphicx}
\RequirePackage{geometry}
\RequirePackage{fancyhdr}
\RequirePackage{xcolor}
\RequirePackage{etoolbox}
\RequirePackage{titlesec}
\RequirePackage{tocloft}
\RequirePackage{hyperref}




% Метаданные журнала — переопределяемые макросы
\newcommand{\journaltitle}{}
\newcommand{\journalissue}{}
\newcommand{\journalyear}{}
\newcommand{\journalurl}{}
\newcommand{\@editorinchief}{}

\newcommand{\SetJournalTitle}[1]{\renewcommand{\journaltitle}{#1}}
\newcommand{\SetJournalIssue}[1]{\renewcommand{\journalissue}{#1}}
\newcommand{\SetJournalYear}[1]{\renewcommand{\journalyear}{#1}}
\newcommand{\SetJournalURL}[1]{\renewcommand{\journalurl}{#1}}

% Простые окружения
\newenvironment{jabstract}{\begin{center}\bfseries Аннотация\end{center}\begin{quotation}}{\end{quotation}}
\newcommand{\keywords}[1]{\par\smallskip\noindent\textbf{Ключевые слова: }#1\par\smallskip}

% Колонтитулы по-умолчанию (можно переопределить в cfg-headers.tex)
\pagestyle{fancy}
\fancyhf{}
\fancyhead[LE]{\small\textcolor{gray}{\thepage}\quad\textit{\journaltitle}}
\fancyhead[RO]{\small\textit{\journaltitle}\quad\textcolor{gray}{\thepage}}
\fancyfoot[C]{\footnotesize\journalissue\ \journalyear\ --- \journalurl}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}

% Упрощённые авторские команды
\newcommand{\Affiliation}[1]{\par\smallskip\noindent\textit{#1}\par}
\newcommand{\AuthorEmail}[1]{\par\smallskip\noindent\texttt{#1}\par}

% Hyperref настройки
\AtBeginDocument{%
  \hypersetup{%
    pdfauthor={\@editorinchief},
    pdftitle={\journaltitle},
    pdfproducer={LaTeX with journal.cls},
    hypertexnames=false,
    pageanchor=false,
    draft=false,
    final=true,
    pdfpagelabels=true,
    unicode=true
  }%
}

\endinput
