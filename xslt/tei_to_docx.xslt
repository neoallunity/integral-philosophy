<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    exclude-result-prefixes="tei">

<!-- DOCX is complex - this creates intermediate format for external processing -->

<xsl:output method="xml" indent="yes" encoding="UTF-8"/>

<xsl:template match="/tei:TEI">
    <document>
        <title><xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/></title>
        <author><xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:name"/></author>
        <date><xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:date"/></date>
        <content>
            <xsl:apply-templates select="tei:text/tei:body"/>
        </content>
    </document>
</xsl:template>

<xsl:template match="tei:div[@type='page']">
    <chapter>
        <title><xsl:value-of select="tei:head"/></title>
        <xsl:apply-templates select="*[not(self::tei:head)]"/>
    </chapter>
</xsl:template>

<xsl:template match="tei:p">
    <paragraph><xsl:apply-templates/></paragraph>
</xsl:template>

<xsl:template match="tei:hi[@rend='bold']">
    <bold><xsl:apply-templates/></bold>
</xsl:template>

<xsl:template match="tei:hi[@rend='it']">
    <italic><xsl:apply-templates/></italic>
</xsl:template>

</xsl:stylesheet>