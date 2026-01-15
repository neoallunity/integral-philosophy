<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:h="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="h">

<xsl:output method="xml" indent="yes" encoding="UTF-8"/>

<xsl:template match="/h:html">
    <TEI>
        <teiHeader>
            <fileDesc>
                <titleStmt>
                    <title><xsl:value-of select="//h:title"/></title>
                    <author><xsl:value-of select="//h:meta[@name='author']/@content"/></author>
                </titleStmt>
                <publicationStmt>
                    <publisher>Generated from HTML</publisher>
                    <date><xsl:value-of select="//h:meta[@name='date']/@content"/></date>
                </publicationStmt>
                <sourceDesc>
                    <bibl>
                        <title><xsl:value-of select="//h:title"/></title>
                        <url>Original HTML</url>
                    </bibl>
                </sourceDesc>
            </fileDesc>
            <profileDesc>
                <langUsage>
                    <language ident="{//h:html/@lang}"/>
                </langUsage>
            </profileDesc>
        </teiHeader>
        <text>
            <body>
                <xsl:apply-templates select="//h:body"/>
            </body>
        </text>
    </TEI>
</xsl:template>

<xsl:template match="h:section | h:article | h:main">
    <div type="section">
        <xsl:attribute name="xml:id">
            <xsl:value-of select="@id"/>
        </xsl:attribute>
        <xsl:apply-templates/>
    </div>
</xsl:template>

<xsl:template match="h:header">
    <div type="header">
        <xsl:apply-templates/>
    </div>
</xsl:template>

<xsl:template match="h:footer">
    <div type="footer">
        <xsl:apply-templates/>
    </div>
</xsl:template>

<xsl:template match="h:nav">
    <div type="navigation">
        <xsl:apply-templates/>
    </div>
</xsl:template>

<xsl:template match="h:aside">
    <div type="aside">
        <xsl:apply-templates/>
    </div>
</xsl:template>

<xsl:template match="h:h1">
    <head type="h1">
        <xsl:apply-templates/>
    </head>
</xsl:template>

<xsl:template match="h:h2">
    <head type="h2">
        <xsl:apply-templates/>
    </head>
</xsl:template>

<xsl:template match="h:h3">
    <head type="h3">
        <xsl:apply-templates/>
    </head>
</xsl:template>

<xsl:template match="h:h4|h:h5|h:h6">
    <head>
        <xsl:value-of select="local-name()"/>
        <xsl:text>: </xsl:text>
        <xsl:apply-templates/>
    </head>
</xsl:template>

<xsl:template match="h:p">
    <p>
        <xsl:apply-templates/>
    </p>
</xsl:template>

<xsl:template match="h:div">
    <div>
        <xsl:if test="@class">
            <xsl:attribute name="type">
                <xsl:value-of select="@class"/>
            </xsl:attribute>
        </xsl:if>
        <xsl:if test="@id">
            <xsl:attribute name="xml:id">
                <xsl:value-of select="@id"/>
            </xsl:attribute>
        </xsl:if>
        <xsl:apply-templates/>
    </div>
</xsl:template>

<xsl:template match="h:span">
    <span>
        <xsl:if test="@class">
            <xsl:attribute name="type">
                <xsl:value-of select="@class"/>
            </xsl:attribute>
        </xsl:if>
        <xsl:apply-templates/>
    </span>
</xsl:template>

<xsl:template match="h:a">
    <ref>
        <xsl:attribute name="target">
            <xsl:value-of select="@href"/>
        </xsl:attribute>
        <xsl:apply-templates/>
    </ref>
</xsl:template>

<xsl:template match="h:img">
    <figure>
        <graphic>
            <xsl:attribute name="url">
                <xsl:value-of select="@src"/>
            </xsl:attribute>
            <xsl:if test="@alt">
                <xsl:attribute name="alt">
                    <xsl:value-of select="@alt"/>
                </xsl:attribute>
            </xsl:if>
        </graphic>
        <xsl:if test="@alt">
            <figDesc><xsl:value-of select="@alt"/></figDesc>
        </xsl:if>
    </figure>
</xsl:template>

<xsl:template match="h:ul|h:ol">
    <list>
        <xsl:if test="local-name()='ol'">
            <xsl:attribute name="type">ordered</xsl:attribute>
        </xsl:if>
        <xsl:apply-templates/>
    </list>
</xsl:template>

<xsl:template match="h:li">
    <item>
        <xsl:apply-templates/>
    </item>
</xsl:template>

<xsl:template match="h:blockquote">
    <quote>
        <xsl:apply-templates/>
    </quote>
</xsl:template>

<xsl:template match="h:pre">
    <code>
        <xsl:attribute name="lang">text</xsl:attribute>
        <xsl:value-of select="."/>
    </code>
</xsl:template>

<xsl:template match="h:code">
    <hi rend="t">
        <xsl:value-of select="."/>
    </hi>
</xsl:template>

<xsl:template match="h:strong|h:b">
    <hi rend="bold">
        <xsl:apply-templates/>
    </hi>
</xsl:template>

<xsl:template match="h:em|h:i">
    <hi rend="it">
        <xsl:apply-templates/>
    </hi>
</xsl:template>

<xsl:template match="h:table">
    <table>
        <xsl:apply-templates/>
    </table>
</xsl:template>

<xsl:template match="h:tr">
    <row>
        <xsl:apply-templates/>
    </row>
</xsl:template>

<xsl:template match="h:td|h:th">
    <cell>
        <xsl:if test="local-name()='th'">
            <xsl:attribute name="role">header</xsl:attribute>
        </xsl:if>
        <xsl:apply-templates/>
    </cell>
</xsl:template>

<!-- Math elements -->
<xsl:template match="h:math">
    <formula notation="mathml">
        <xsl:copy-of select="node()"/>
    </formula>
</xsl:template>

<!-- Script and style elements (ignored for TEI) -->
<xsl:template match="h:script|h:style"/>

</xsl:stylesheet>