<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:import href="/usr/local/share/xsl/docbook/html/chunk.xsl"/>

  <xsl:template match="hafunsynopsis">
    <xsl:call-template name="inline.monoseq"/>    
  </xsl:template>

  <xsl:template match="hafunsynopsis/function">
    <xsl:apply-templates/><xsl:text> :: </xsl:text>
  </xsl:template>

  <xsl:template match="hatyfun">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="hatyfun/hatyfun[1]">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>) -&gt; </xsl:text>  
  </xsl:template>

  <xsl:template match="hatyfun/*[1]">
    <xsl:apply-templates/><xsl:text> -&gt; </xsl:text>  
  </xsl:template>

  <xsl:template match="hatyapp">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="hatyapp/hatyapp[1]">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>) </xsl:text>  
  </xsl:template>

  <xsl:template match="hatyapp/*[1]">
    <xsl:apply-templates/><xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="hatycon">
    <xsl:call-template name="inline.charseq"/>
  </xsl:template>

  <xsl:template match="hatyvar">
    <xsl:call-template name="inline.charseq"/>
  </xsl:template>

  <xsl:template match="hatylst">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="hatylst/*">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>]</xsl:text>
  </xsl:template>

  <xsl:template match="hatypar">
    <xsl:text>()</xsl:text>
  </xsl:template>

  <xsl:template match="hatypar/*[position()!=last()]">
    <xsl:apply-templates/>
    <xsl:text>, </xsl:text>
  </xsl:template>

  <xsl:template match="hatypar/*[position()!=last() and position()=1]">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>, </xsl:text>
  </xsl:template>

  <xsl:template match="hatypar/*[position()=last()]">
    <xsl:apply-templates/>
    <xsl:text>)</xsl:text>
  </xsl:template>

</xsl:stylesheet>
