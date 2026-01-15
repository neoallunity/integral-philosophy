<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    exclude-result-prefixes="tei">

<xsl:output method="xml" 
    doctype-public="-//W3C//DTD XHTML 1.1//EN"
    doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"
    indent="yes" encoding="UTF-8"/>

<xsl:template match="/tei:TEI">
    <html xml:lang="{tei:text/@xml:lang}">
        <head>
            <title><xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/></title>
            <meta charset="UTF-8"/>
            <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            <style>
                body { font-family: serif; line-height: 1.6; max-width: 800px; margin: 0 auto; padding: 20px; }
                .metadata { background: #f0f0f0; padding: 1em; margin: 1em 0; border-radius: 4px; }
                .section { border-bottom: 1px solid #ccc; margin-bottom: 2em; }
                table { border-collapse: collapse; width: 100%; }
                th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
                th { background-color: #f2f2f2; }
            </style>
        </head>
        <body>
            <header class="metadata">
                <h1><xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/></h1>
                <p>Author: <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:name"/></p>
                <p>Date: <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:date"/></p>
            </header>
            
            <main>
                <xsl:apply-templates select="tei:text/tei:body"/>
            </main>
        </body>
    </html>
</xsl:template>

<xsl:template match="tei:div[@type='section']">
    <section class="section">
        <xsl:if test="@xml:id">
            <xsl:attribute name="id">
                <xsl:value-of select="@xml:id"/>
            </xsl:attribute>
        </xsl:if>
        <xsl:apply-templates/>
    </section>
</xsl:template>

<xsl:template match="tei:div">
    <div>
        <xsl:if test="@type">
            <xsl:attribute name="class">
                <xsl:value-of select="@type"/>
            </xsl:attribute>
        </xsl:if>
        <xsl:if test="@xml:id">
            <xsl:attribute name="id">
                <xsl:value-of select="@xml:id"/>
            </xsl:attribute>
        </xsl:if>
        <xsl:apply-templates/>
    </div>
</xsl:template>

<xsl:template match="tei:head[@type='h1']">
    <h1><xsl:apply-templates/></h1>
</xsl:template>

<xsl:template match="tei:head[@type='h2']">
    <h2><xsl:apply-templates/></h2>
</xsl:template>

<xsl:template match="tei:head[@type='h3']">
    <h3><xsl:apply-templates/></h3>
</xsl:template>

<xsl:template match="tei:head">
    <h4><xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="tei:p">
    <p><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="tei:ref">
    <a href="{@target}"><xsl:apply-templates/></a>
</xsl:template>

<xsl:template match="tei:figure">
    <figure>
        <img src="{tei:graphic/@url}">
            <xsl:if test="tei:graphic/@alt">
                <xsl:attribute name="alt">
                    <xsl:value-of select="tei:graphic/@alt"/>
                </xsl:attribute>
            </xsl:if>
        </img>
        <xsl:if test="tei:figDesc">
            <figcaption><xsl:value-of select="tei:figDesc"/></figcaption>
        </xsl:if>
    </figure>
</xsl:template>

<xsl:template match="tei:list">
    <xsl:choose>
        <xsl:when test="@type='ordered'">
            <ol><xsl:apply-templates/></ol>
        </xsl:when>
        <xsl:otherwise>
            <ul><xsl:apply-templates/></ul>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template match="tei:item">
    <li><xsl:apply-templates/></li>
</xsl:template>

<xsl:template match="tei:quote">
    <blockquote><xsl:apply-templates/></blockquote>
</xsl:template>

<xsl:template match="tei:code">
    <pre><xsl:apply-templates/></pre>
</xsl:template>

<xsl:template match="tei:hi[@rend='bold']">
    <strong><xsl:apply-templates/></strong>
</xsl:template>

<xsl:template match="tei:hi[@rend='it']">
    <em><xsl:apply-templates/></em>
</xsl:template>

<xsl:template match="tei:hi[@rend='t']">
    <code><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="tei:table">
    <table>
        <xsl:apply-templates/>
    </table>
</xsl:template>

<xsl:template match="tei:row">
    <tr><xsl:apply-templates/></tr>
</xsl:template>

<xsl:template match="tei:cell">
    <xsl:choose>
        <xsl:when test="@role='header'">
            <th><xsl:apply-templates/></th>
        </xsl:when>
        <xsl:otherwise>
            <td><xsl:apply-templates/></td>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

</xsl:stylesheet>