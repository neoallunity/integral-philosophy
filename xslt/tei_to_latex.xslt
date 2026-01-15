<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="tei">

<xsl:output method="text" encoding="UTF-8"/>

<xsl:template match="/tei:TEI">
    <xsl:text>\documentclass[12pt,a4paper]{article}
\usepackage[T2A,T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[russian,english]{babel}
\usepackage{amsmath,amssymb}
\usepackage{graphicx}
\usepackage{hyperref}
\usepackage{geometry}
\usepackage{fancyhdr}
\usepackage{setspace}

\geometry{a4paper,margin=2.5cm}
\linespread{1.5}

\title{</xsl:text>
    <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
    <xsl:text>}
\author{</xsl:text>
    <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:name"/>
    <xsl:text>}
\date{</xsl:text>
    <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:date"/>
    <xsl:text>}

\begin{document}
\maketitle
\tableofcontents
\newpage

</xsl:text>
    
    <xsl:apply-templates select="tei:text/tei:body"/>
    
    <xsl:text>
\end{document}
</xsl:text>
</xsl:template>

<xsl:template match="tei:div[@type='page']">
    <xsl:text>\section*{</xsl:text>
    <xsl:value-of select="tei:head"/>
    <xsl:text>}
</xsl:text>
    <xsl:apply-templates select="*[not(self::tei:head)]"/>
    <xsl:text>\newpage
</xsl:text>
</xsl:template>

<xsl:template match="tei:div[@type='metadata']">
    <xsl:text>\begin{quote}
\textbf{Metadata:}
</xsl:text>
    <xsl:for-each select="tei:p">
        <xsl:text>\textit{</xsl:text>
        <xsl:value-of select="substring-before(., ':')"/>
        <xsl:text>:} </xsl:text>
        <xsl:value-of select="substring-after(., ':')"/>
        <xsl:text>\\
</xsl:text>
    </xsl:for-each>
    <xsl:text>\end{quote}
</xsl:text>
</xsl:template>

<xsl:template match="tei:div[@type='links']">
    <xsl:text>\textbf{Links:}
\begin{itemize}
</xsl:text>
    <xsl:apply-templates select="tei:list/tei:item"/>
    <xsl:text>\end{itemize}
</xsl:text>
</xsl:template>

<xsl:template match="tei:p">
    <xsl:text></xsl:text>
    <xsl:apply-templates/>
    <xsl:text>

</xsl:text>
</xsl:template>

<xsl:template match="tei:ref">
    <xsl:text>\href{</xsl:text>
    <xsl:value-of select="@target"/>
    <xsl:text>}{</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:formula">
    <xsl:text>\begin{equation}
</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>\end{equation}
</xsl:text>
</xsl:template>

<xsl:template match="tei:quote[@type='code']">
    <xsl:text>\begin{verbatim}
</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>\end{verbatim}
</xsl:text>
</xsl:template>

<xsl:template match="tei:list">
    <xsl:text>\begin{itemize}
</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\end{itemize}
</xsl:text>
</xsl:template>

<xsl:template match="tei:item">
    <xsl:text>\item </xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="tei:quote">
    <xsl:text>\begin{quote}
\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}
\end{quote}
</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='bold']">
    <xsl:text>\textbf{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='it']">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='t']">
    <xsl:text>\texttt{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
</xsl:template>

</xsl:stylesheet>