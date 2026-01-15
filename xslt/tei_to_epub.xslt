<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    exclude-result-prefixes="tei">

<xsl:output method="xml" indent="yes" encoding="UTF-8"/>

<!-- EPUB specific - create separate chapters -->
<xsl:template match="/tei:TEI">
    <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
            <title><xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/></title>
            <meta charset="UTF-8"/>
            <style>
                body { font-family: serif; line-height: 1.4; margin: 1em; }
                .chapter { page-break-before: always; }
                .metadata { background: #f0f0f0; padding: 0.5em; margin: 1em 0; }
                h1 { color: #333; border-bottom: 2px solid #333; }
                h2 { color: #666; }
            </style>
        </head>
        <body>
            <div class="front-matter">
                <h1><xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/></h1>
                <p>By <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:name"/></p>
                <p><xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:date"/></p>
            </div>
            
            <div class="table-of-contents">
                <h2>Table of Contents</h2>
                <ul>
                    <xsl:for-each select="tei:text/tei:body/tei:div[@type='page']">
                        <li><a href="#{generate-id(.)}"><xsl:value-of select="tei:head"/></a></li>
                    </xsl:for-each>
                </ul>
            </div>
            
            <xsl:apply-templates select="tei:text/tei:body"/>
        </body>
    </html>
</xsl:template>

<xsl:template match="tei:div[@type='page']">
    <div class="chapter" id="{generate-id(.)}">
        <h1><xsl:value-of select="tei:head"/></h1>
        <xsl:apply-templates select="*[not(self::tei:head)]"/>
    </div>
</xsl:template>

<xsl:template match="tei:div[@type='metadata']">
    <div class="metadata">
        <xsl:apply-templates/>
    </div>
</xsl:template>

<xsl:template match="tei:p">
    <p><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="tei:ref">
    <a href="{@target}"><xsl:value-of select="."/></a>
</xsl:template>

<xsl:template match="tei:list">
    <ul>
        <xsl:apply-templates/>
    </ul>
</xsl:template>

<xsl:template match="tei:item">
    <li><xsl:apply-templates/></li>
</xsl:template>

<xsl:template match="tei:hi[@rend='bold']">
    <strong><xsl:apply-templates/></strong>
</xsl:template>

<xsl:template match="tei:hi[@rend='it']">
    <em><xsl:apply-templates/></em>
</xsl:template>

</xsl:stylesheet>