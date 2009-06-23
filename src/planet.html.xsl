<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:atom="http://www.w3.org/2005/Atom"
                xmlns:restas="restas://restas/"
                xmlns:xi="http://www.w3.org/2001/XInclude"
                extension-element-prefixes="restas"
                version="1.0">

    <xsl:template match="/atom:feed">
        <overlay>
            <head>
                <link href="/{$baseurl}/css/planet.css" rel="stylesheet" type="text/css" />
                <link rel="alternate" href="/{$baseurl}/atom.xml" title="Планета блогов о Lisp" type="application/atom+xml" />
                <title>Russian Lisp Planet</title>
            </head>
            
            <div id="content">
                <div id="planet-body">
                    <div id="planet-info-panel">
                        <div id="syndicate">
                            <a href="/{$baseurl}/atom.xml">Подписаться</a>
                        </div>
                        
                        <div id="suggest">
                            <a href="mailto:lisp@catap.ru">Предложить блог</a>
                        </div>
                        
                        <h3>Авторы:</h3>
                        <ul id="authors">
                            <xsl:for-each  select="document('../feeds.xml')//xi:include" >
                                <xsl:sort select="@name" data-type="text" />

                                <li>
                                    <a href="{@link}">
                                        <xsl:value-of select="@name" />
                                    </a>
                                </li>
                            </xsl:for-each>
                        </ul>
                    </div>

                    <div id="planet-content">
                        <xsl:apply-templates select="atom:entry" />
                    </div>
                </div>
            </div>
        </overlay>
    </xsl:template>

    <xsl:template match="atom:entry">
        <div class="entry">
            <div class="entry-title">
                <a href="{atom:link/@href}">
                    <xsl:value-of select="atom:title" />
                </a>
                <div class="entry-author-info">
                    <strong>Источник: </strong>
                    <a href="{atom:source/atom:link/@href}">
                        <xsl:value-of select="atom:source/atom:title" />
                    </a>
                </div>
            </div>

            <div class="entry-content">
                <restas:text2html select="atom:content" />
            </div>
        </div>
    </xsl:template>
    
</xsl:stylesheet>