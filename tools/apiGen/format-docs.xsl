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
<xref-other><xsl:value-of select="@linkend"/></xref-other>
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

<xsl:template match="para[not(example) and not(informalexample)]">
<para><xsl:apply-templates/></para>
</xsl:template>

<xsl:template match="para[informalexample]">
<para><xsl:apply-templates select="node()[name()!='informalexample']"/></para>
<xsl:apply-templates select="informalexample"/>
</xsl:template>

<xsl:template match="para[example]">
<para><xsl:apply-templates select="node()[name()!='example']"/></para>
<xsl:apply-templates select="example"/>
</xsl:template>

<xsl:template match="varlistentry">
<definition>
	<term><xsl:apply-templates select="term"/></term>
	<xsl:apply-templates select="listitem/para/child::node()"/>
</definition>
</xsl:template>

<xsl:template match="itemizedlist/listitem/para">
<listitem><xsl:apply-templates/></listitem>
</xsl:template>

<xsl:template match="simplelist/member">
<listitem><xsl:apply-templates/></listitem>
</xsl:template>

<xsl:template match="informaltable[tgroup/tbody/row]">
<para><xsl:apply-templates/></para>
</xsl:template>

<xsl:template match="tgroup/tbody/row">
<definition>
	<term><xsl:apply-templates select="entry[1]"/></term>
	<xsl:apply-templates select="entry[position()>1]"/>
</definition>
</xsl:template>

<xsl:template match="keycombo">
<xsl:value-of select="keycap[1]"/>-<xsl:value-of select="keycap[2]"/>
</xsl:template>

<xsl:template match="section | refsect2">
<section>
	<title><xsl:value-of select="title"/></title>
	<xsl:apply-templates select="para | programlisting | example"/>
</section>
</xsl:template>

<xsl:template match="example">
<example>
	<title><xsl:value-of select="title"/></title>
	<xsl:apply-templates select="para | programlisting | informaltable"/>
</example>
</xsl:template>

<xsl:template match="programlisting">
<programlisting><xsl:value-of select="."/></programlisting>
</xsl:template>

<xsl:template match="footnote"></xsl:template>

<xsl:template match="/apidoc">
  <apidoc>
  <xsl:for-each select="book">
  <module>
  <!-- top level module information -->
  <module-info>
    <name><xsl:value-of select="refentry/refnamediv/refname"/></name>
    <altname><xsl:value-of select="refentry/refsynopsisdiv/anchor/@id"/></altname>
    <summary><xsl:value-of select="refentry/refnamediv/refpurpose"/></summary>
    <description>
      <xsl:for-each select="refentry/refsect1[title='Description']">
        <xsl:apply-templates select="para | section | refsect2"/>
      </xsl:for-each>
    </description>
    <object-hierarchy>
      <xsl:for-each select="refentry/refsect1[title='Object Hierarchy']/synopsis/node()">
        <xsl:choose>
	  <xsl:when test="name(.)='link'">
	    <xref-type><xsl:value-of select="."/></xref-type>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="."/>
	    <xsl:value-of select="name(.)"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </object-hierarchy>
  </module-info>
  <!-- Function documentation -->
  <xsl:for-each select="refentry/refsect1[title='Details']/refsect2[contains(title,' ()')]">
    <function>
      <name><xsl:value-of select="indexterm/primary"/></name>
      <since>
               <xsl:value-of select="normalize-space(substring-after(para[starts-with(text(),'Since')], 'Since'))"/>
      </since>
      <doc>
	<xsl:apply-templates select="para[not(starts-with(text(),'Since')) and normalize-space(text())!='']"/>
      </doc>
      <params>
	<xsl:for-each select="variablelist/varlistentry">
	  <param>
	    <name><xsl:value-of select="term/parameter | term/emphasis"/></name>
	    <xsl:apply-templates select="listitem/simpara"/>
	  </param>
	</xsl:for-each>
      </params>
    </function>
  </xsl:for-each>
  <!-- Properties documentation -->
<!--
  <xsl:for-each select="refentry/refsect1[title='Properties']/variablelist/varlistentry">
    <property>
      <name><xsl:value-of select="term/literal"/></name>
      <since>
        <xsl:value-of select="normalize-space(substring-after(listitem/para[starts-with(text(),'Since')], 'Since'))"/>
      </since>
      <doc>
        <xsl:apply-templates select="listitem/para[not(starts-with(text(),'Since')) and normalize-space(text())!='']"/>
      </doc>
    </property>
  </xsl:for-each>
-->
  <!-- Properties documentation (new formatting) -->
  <xsl:for-each select="refentry/refsect1[title='Properties']/refsect2">
    <property>
      <name><xsl:value-of select="substring-before(substring-after(title,'&quot;'),'&quot;')"/></name>
      <since>
        <xsl:value-of select="normalize-space(substring-after(para[starts-with(text(),'Since')], 'Since'))"/>
      </since>
      <doc>
        <xsl:apply-templates select="para[not(starts-with(text(),'Since')) and normalize-space(text())!='']"/>
      </doc>
    </property>
  </xsl:for-each>
  <!-- Signals documentation -->
  <xsl:for-each select="refentry/refsect1[title='Signals']/refsect2">
    <signal>
      <name><xsl:value-of select="substring-before(substring-after(title,'&quot;'),'&quot;')"/></name>
      <since>
        <xsl:value-of select="normalize-space(substring-after(para[starts-with(text(),'Since')], 'Since'))"/>
      </since>
      <doc>
        <xsl:apply-templates select="para[not(starts-with(text(),'Since')) and normalize-space(text())!='']"/>
      </doc>
      <params>
        <xsl:for-each select="variablelist/varlistentry">
          <param>
            <name><xsl:value-of select="term/parameter"/></name>
            <xsl:apply-templates select="listitem/simpara"/>
          </param>
        </xsl:for-each>
      </params>
    </signal>
  </xsl:for-each>
  </module>
  </xsl:for-each>
  </apidoc>
</xsl:template>
</xsl:transform>

