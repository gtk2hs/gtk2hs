<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<xsl:transform
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  <xsl:output method="xml" indent="yes"/>

<xsl:template match="link/function">
<xref-func><xsl:value-of select="."/></xref-func>
</xsl:template>

<xsl:template match="link/type">
<xref-type><xsl:value-of select="."/></xref-type>
</xsl:template>

<xsl:template match="xref">
<xref-other><xsl:value-of select="."/></xref-other>
</xsl:template>

<xsl:template match="emphasis">
<emphasis><xsl:value-of select="."/></emphasis>
</xsl:template>

<xsl:template match="literal">
<literal><xsl:value-of select="."/></literal>
</xsl:template>

<xsl:template match="parameter">
<arg><xsl:value-of select="."/></arg>
</xsl:template>

<xsl:template match="/">
  <apidoc>
  <xsl:for-each select="/book/refentry/refsect1[title='Details']/refsect2[contains(title,' ()')]">
    <function>
      <name><xsl:value-of select="indexterm/primary"/></name>
      <since>
               <xsl:value-of select="number(substring-after(para[starts-with(text(),'Since')], 'Since  '))"/>
      </since>
      <doc>
	 <xsl:for-each select="para[not(starts-with(text(),'Since')) and normalize-space(text())!='']">
	    <!--<xsl:copy-of select="."/>-->
	    <para><xsl:apply-templates/></para>
	 </xsl:for-each>
      </doc>
    </function>
  </xsl:for-each>
  </apidoc>
</xsl:template>
</xsl:transform>

