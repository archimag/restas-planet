<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:atom="http://www.w3.org/2005/Atom"
                xmlns:exsl="http://exslt.org/common"
                xmlns:planet="chrome://restas/planet/"
                extension-element-prefixes="exsl"
                version="1.0">

    <xsl:template match="/feeds">
        <feed xmlns="http://www.w3.org/2005/Atom">
            
            <title>Russian Lisp Planet</title>
            <link rel="self" href="http://catap.lisp.ru/planet/atom.xml"/>
            <link href="http://catap.lisp.ru/planet/"/>
            <id>http://catap.lisp.ru/planet/atom.xml</id>

            <xsl:variable name="items">
                <xsl:apply-templates select="rss/channel" />
                <xsl:apply-templates select="atom:feed" />
            </xsl:variable>


            <xsl:variable name="items2">
                <xsl:for-each  select="exsl:node-set($items)/atom:entry">
                    <xsl:sort select="planet:universal-time(atom:published)" order="descending"/>
                    <xsl:copy-of select="." />
                </xsl:for-each>
            </xsl:variable>

            <xsl:for-each  select="exsl:node-set($items2)/atom:entry[position() &lt; 51]">
                <xsl:copy-of select="." />
            </xsl:for-each>

        </feed>

    </xsl:template>

    <xsl:template match="channel">
        <xsl:variable name="author-name" select="title" />
        <xsl:variable  name="author-uri" select="link" />

        <xsl:for-each select="item">
            <xsl:if test="*">
                <entry xmlns="http://www.w3.org/2005/Atom">
                    <title type="html">
                        <xsl:choose>
                            <xsl:when test="title">
                                <xsl:value-of select="title" />
                            </xsl:when>
                            <xsl:otherwise>Безымянное</xsl:otherwise>
                        </xsl:choose>
                    </title>
                    
                    <link href="{link}" />
                    
                    <id><xsl:value-of select="link" /></id>

                    <published>
                        <xsl:value-of select="planet:rfc2822-to-rfc3339(pubDate)" />
                    </published>

                    <content type="html"><xsl:value-of select="description" /></content>

                    <author>
                        <name><xsl:value-of select="$author-name" /></name>
                        <uri><xsl:value-of select="$author-uri" /></uri>
                    </author>

                    <source>
                        <title type="html"><xsl:value-of select="$author-name" /></title>
                        <link rel="self" href="{$author-uri}" />
                        <id><xsl:value-of select="$author-uri" /></id>
                    </source>
                </entry>
            </xsl:if>
        </xsl:for-each>
        
    </xsl:template>

    <xsl:template match="atom:feed">
        <xsl:variable name="feed-name" select="atom:title" />
        <xsl:variable name="feed-link" select="atom:link[not(@rel)]/@href | atom:link[@rel = 'alternate']/@href" />

        <xsl:for-each select="atom:entry">
            <entry xmlns="http://www.w3.org/2005/Atom">
                <title type="html">
                    <xsl:choose>
                        <xsl:when test="atom:title">
                            <xsl:value-of select="atom:title" />
                        </xsl:when>
                        <xsl:otherwise>Безымянное</xsl:otherwise>
                    </xsl:choose>
                </title>

                
                <link href="{atom:link[not(@rel)]/@href | atom:link[@rel = 'alternate']/@href}" />
                
                <xsl:for-each  select="atom:id|atom:updated|atom:published|atom:title">
                    <xsl:copy>
                        <xsl:value-of select="." />
                    </xsl:copy>
                </xsl:for-each>

                <content type="html">
                    <xsl:value-of select="atom:content" />
                </content>


                <xsl:copy-of select="atom:author" />

                <source>
                    <title type="html"><xsl:value-of select="$feed-name" /></title>
                    <link rel="self" href="{$feed-link}" />
                    <id><xsl:value-of select="$feed-link" /></id>
                </source>
            </entry>
        </xsl:for-each>
    </xsl:template>

</xsl:stylesheet>