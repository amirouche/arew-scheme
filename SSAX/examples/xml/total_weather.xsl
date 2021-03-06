<?xml version='1.0'?>
<!-- METAR/SPECI/SYNOP and TAF XSL. This XSL can be used to format
     <Reports> and <Forecasts> elements separately (as root elements of
     separate OMF documents). This XSL also applies when the
     <Reports> and <Forecasts> elements are combined in an OMF-TW element.
     This version can handle both old-style METAR/SPECI markup as well
     as new, fully-parsed one, see OMF-SYNOP.html, v2.0 
     $Id: total_weather.xsl,v 1.1.1.1 2000/03/14 19:38:29 oleg Exp $
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">
<xsl:script><![CDATA[
   prev_bid ="";  // To keep track of the BId attr of the previous element
		  // in the 'xsl:for-each select="/Reports/*"' loop below.
		  // Alas, MS chose not to implement a first-of() XSL method;
		  // Without it, removing duplicates and separating stretches
		  // of the same element are impossible in "pure" XSL

   var tstamp_now = Math.round((new Date()).getTime()/1000); // Epoch secs
	
		// Convert TStamp from Epoch secs string to a better string
   function conv_date(epoch_sec) {
      date = new Date(parseInt(epoch_sec)*1000);
      return (1+ date.getUTCMonth()) + "/" + date.getUTCDate() + " " + 
	  	date.getUTCHours() + ":" + date.getUTCMinutes();
   }

		// Convert a TRange string to a better-looking string
		// TRange is a string of two numbers (epoch secs) 
		// separated by a ", "
   function conv_trange(trange_str) {
      var t_start = new Date(parseInt(trange_str)*1000);
      var t_end = new Date(parseInt(
			trange_str.substr(trange_str.indexOf(", ")+2)
			  )*1000);
      return t_start.getUTCDate() + " " + 
	     t_start.getUTCHours() + ":" + t_start.getUTCMinutes() +
	     " - " + t_end.getUTCDate() + " " + 
	     t_end.getUTCHours() + ":" + t_end.getUTCMinutes();
   }

		// Convert Vis (if specified and not INF) from meters to
		// Statute Miles
		// Return a string "Visibility: nnn SM"
   function conv_vis(vis_node) {
      if( !vis_node ) return "";	// Not given
      var vis_text = vis_node.text;
      if( vis_text == "INF" ) return "Visibility: INF";
      var vis = parseInt(vis_text)/1610; // in SM
      return "Visibility: " + 
	Math.round(100*vis)/100 + " SM";
   }
  

		// Given values for visibility and ceiling, return a string
		// FL-cat-OK or FL-cat-ATT
		// depending on the values of the arguments.
		// This string will be used as the value of the class
		// attribute in a generated HTML element
   function set_FL_cat_class(vis_node,ceiling_node) {
	var vis_threshold =  4830;	// 3 miles, For IFR condition
	var ceil_threshold = 1000;	// 1000 ft
	if( (vis_node && vis_node.text != "INF" && parseInt(vis_node.text) <= vis_threshold) ||
	    (ceiling_node && ceiling_node.text != "INF" && 
		parseInt(ceiling_node.text) <= ceil_threshold)  ){
	   return "FL-cat-ATT"
	}
	return "FL-cat-OK"
   }

		// Given a TRange string (see above), return either
		// "per_c" or "per_nc" 
		// depending if the TRange interval includes the current
		// moment (the value of the tstamp_now, see above).
		// This string will be used as the value of a class
		// attribute in a generated HTML element
   function period_class(trange_str) {
      var t_start = parseInt(trange_str);
      if( t_start > tstamp_now ) return "per_nc";
      var t_end = parseInt(
			trange_str.substr(trange_str.indexOf(", ")+2));
      return t_end >= tstamp_now ? "per_c" : "per_nc";
   }

	
		// Given values for a Wind attribute, return a string
		// Wind-cat-OK or Wind-cat-ATT
		// depending on the maximum wind speed.
		// This string will be used as the value of a class
		// attribute in a generated HTML element
		// The format of the Wind attribute is
		// 	"dir[-dirmax], speed[-speedmax]"
   var wind_speed_threshold =  10;	// 10 m/sec, that is, 20 KT
   function set_wind_class(wind_node) {
	if(!wind_node) return "Wind-cat-OK";
	var speed = parseFloat(wind_node.text.match(/([0-9.]+)$/)[1]);
	return (!isNaN(speed) && speed >= wind_speed_threshold ) ?
		"Wind-cat-ATT" : "Wind-cat-OK";
   }

		// Format the wind attribute: "dir[-dirmax], speed[-speedmax]"
		// into a more presentable string. In particular, convert
		// wind speed to KT (from m/sec). The conversion factor from m/s
		// to KT is 2
   function present_wind(wind_node) {
	if(!wind_node) return "";
	var data = wind_node.text.match(/^([^,]+), ([0-9.]+)-?([0-9.]*)$/);
	var speed1 = parseFloat(data[2]);
	if( isNaN(speed1) ) return "";
	var speed2 = parseFloat(data[3]);
	if( speed1 == 0 && isNaN(speed2) ) return "Wind: calm";
	return "Wind: from " + data[1] + " @ " + Math.round(2*speed1) + " KT" +
		(isNaN(speed2) ? "" : " G " + Math.round(2*speed2) + " KT"); 
   }
]]></xsl:script>

<xsl:template match="/">
  <xsl:apply-templates/>
</xsl:template>

<!-- A Reports element as a Root element -->
<xsl:template match="/Reports">
<HTML><HEAD><TITLE>Land Surface Observations</TITLE>
<STYLE TYPE="text/css">
	.FL-cat-OK { font-size: 70%; leading: 200%; color: gray; }
	.FL-cat-ATT { font-size: 70%; leading: 200%; color: red; }
	.Wind-cat-OK { font-size: 70%; leading: 200%; color: gray; }
	.Wind-cat-ATT { font-size: 70%; leading: 200%; color: red; }
</STYLE>
</HEAD><BODY BGCOLOR="white">

<H2>Land Surface Observations</H2>
<xsl:for-each select="/Reports/*" order-by="@BId; -number(@TStamp)">
    <xsl:if expr="prev_bid == this.selectSingleNode('@BId').text ? 0 : 
		  prev_bid = this.selectSingleNode('@BId').text">
       <P><BR/></P>
       <TABLE BGCOLOR="#CCCCCC" width="100%" cellpadding="3">
       <TR>
	 <TD><xsl:value-of select="@SName"/></TD>
	 <TD>Id: <xsl:value-of select="@BId"/></TD>
	 <TD>[<xsl:value-of select="@LatLon"/>]</TD>
	 <TD>Elev: <xsl:value-of select="@Elev"/></TD>
       </TR></TABLE>
    </xsl:if>
    <TABLE width="100%" cellpadding="1" BORDER="1" RULES="none" FRAME="hsides">
       <COLGROUP><COL WIDTH="9%"/><COL WIDTH="12%"/><COL/></COLGROUP>
       <xsl:apply-templates select="."/>
    </TABLE>
</xsl:for-each>
</BODY></HTML>
</xsl:template>

<!-- A Forecasts element as a Root element -->
<xsl:template match="/Forecasts">
<HTML><HEAD><TITLE>Terminal Aerodrome Forecasts</TITLE>
<STYLE TYPE="text/css">
	.per_c  { padding-left:1.3em;text-indent:-1em; margin-left: 1em; border-left: medium solid red }
	.per_nc { padding-left:1.5em; margin-left:1em; text-indent:-1em; }
	.var    { padding-left:1.5em; margin-left:2em; text-indent:-1em;}
</STYLE>
</HEAD><BODY BGCOLOR="white">

<H2>Terminal Aerodrome Forecasts</H2>
<xsl:apply-templates/>
</BODY></HTML>
</xsl:template>

<!-- A Combined Total Weather report -->
<xsl:template match="/OMF-TW">
<HTML><HEAD><TITLE>Total Weather Report</TITLE>
<STYLE TYPE="text/css">
	.FL-cat-OK { font-size: 70%; leading: 200%; color: gray; }
	.FL-cat-ATT { font-size: 70%; leading: 200%; color: red; }
	.Wind-cat-OK { font-size: 70%; leading: 200%; color: gray; }
	.Wind-cat-ATT { font-size: 70%; leading: 200%; color: red; }
	.per_c  { padding-left:1.3em;text-indent:-1em; margin-left: 1em; border-left: medium solid red }
	.per_nc { padding-left:1.5em; margin-left:1em; text-indent:-1em; }
	.var    { padding-left:1.5em; margin-left:2em; text-indent:-1em;}
</STYLE>
</HEAD><BODY BGCOLOR="white">

<H2>Total Weather Report</H2>
<xsl:for-each select="*/*" order-by="@BId; -number(@TStamp)">
    <xsl:if expr="prev_bid == this.selectSingleNode('@BId').text ? 0 : 
		  prev_bid = this.selectSingleNode('@BId').text">
       <P><BR/></P>
       <TABLE BGCOLOR="#CCCCCC" width="100%" cellpadding="3">
       <TR>
	 <TD><xsl:value-of select="@SName"/></TD>
	 <TD>Id: <xsl:value-of select="@BId"/></TD>
	 <TD>[<xsl:value-of select="@LatLon"/>]</TD>
       </TR></TABLE>
    </xsl:if>
    <TABLE width="100%" cellpadding="1" BORDER="1" RULES="none" FRAME="hsides">
       <COLGROUP><COL WIDTH="9%"/><COL WIDTH="12%"/><COL/></COLGROUP>
       <xsl:apply-templates select="."/>
    </TABLE>
</xsl:for-each>
</BODY></HTML>
</xsl:template>


<xsl:template match="METAR">
<TR>
<TD><B>METAR</B></TD>
<TD><xsl:eval>conv_date(this.selectSingleNode('@TStamp').text)</xsl:eval></TD>
<TD><xsl:value-of />
  <DIV><xsl:attribute name="class">
	<xsl:eval>set_FL_cat_class(this.selectSingleNode('@Vis'),this.selectSingleNode('@Ceiling'))
	</xsl:eval>
       </xsl:attribute>
       <xsl:eval>conv_vis(this.selectSingleNode('@Vis'))</xsl:eval>
       <xsl:if test="@Ceiling">   Ceiling: <xsl:value-of select="@Ceiling"/></xsl:if>
  </DIV>
</TD></TR>
</xsl:template>

<xsl:template match="SPECI">
<TR>
<TD><B>SPECI</B></TD>
<TD><xsl:eval>conv_date(this.selectSingleNode('@TStamp').text)</xsl:eval></TD>
<TD><xsl:value-of />
  <DIV><xsl:attribute name="class">
	<xsl:eval>set_FL_cat_class(this.selectSingleNode('@Vis'),this.selectSingleNode('@Ceiling'))
	</xsl:eval>
       </xsl:attribute>
       <xsl:eval>conv_vis(this.selectSingleNode('@Vis'))</xsl:eval>
       <xsl:if test="@Ceiling">   Ceiling: <xsl:value-of select="@Ceiling"/></xsl:if>
  </DIV>
</TD></TR>
</xsl:template>

<xsl:template match="SYN">
<TR>
<TD><B>
   <xsl:value-of select="@Title"/>
   <!--
   <xsl:apply-templates select="@Title"/>
    <xsl:choose>
      <xsl:when test="@Title[. = 'AAXX']">SYNOP</xsl:when>
      <xsl:otherwise><xsl:value-of select="@Title"/></xsl:otherwise>
    </xsl:choose>
   -->
   </B></TD>
<TD><xsl:eval>conv_date(this.selectSingleNode('@TStamp').text)</xsl:eval></TD>
<TD><xsl:value-of select="SYID"/>
  <xsl:value-of select="SYG"/>
  <xsl:value-of select="SYCODE"/>
  <DIV>
  <SPAN><xsl:attribute name="class">
	<xsl:eval>set_FL_cat_class(this.selectSingleNode('SYG/@Vis'),this.selectSingleNode('SYG/@Ceiling'))
	</xsl:eval>
       </xsl:attribute>
       <xsl:eval>conv_vis(this.selectSingleNode('SYG/@Vis'))</xsl:eval>
       <xsl:if test="SYG/@Ceiling">   Ceiling: <xsl:value-of select="SYG/@Ceiling"/></xsl:if>
  </SPAN>
  <SPAN><xsl:attribute name="class">
	<xsl:eval>set_wind_class(this.selectSingleNode('SYG/@Wind'))
	</xsl:eval>
       </xsl:attribute>
       <xsl:eval>present_wind(this.selectSingleNode('SYG/@Wind'))</xsl:eval>
  </SPAN>
  </DIV>
</TD></TR>
</xsl:template>

<!--
<xsl:template match="@Title[. = 'AAXX']">
SYNOP
</xsl:template>
<xsl:template match="@Title">
<xsl:value-of select="."/>
</xsl:template>
-->

<xsl:template match="TAF">
  <xsl:if expr="prev_bid == 0"> <!-- when not a part of a total weather report -->
  <P><BR/></P>
  <TABLE BGCOLOR="#CCCCCC" width="100%" cellpadding="3">
  <TR>
    <TD><xsl:value-of select="@SName"/></TD>
    <TD>Id: <xsl:value-of select="@BId"/></TD>
    <TD>[<xsl:value-of select="@LatLon"/>]</TD>
  </TR></TABLE>
  </xsl:if>

  <TABLE width="100%" cellpadding="1" BORDER="1" RULES="none" FRAME="hsides">
  <TR>
  <TD WIDTH="20%">
    <xsl:eval>conv_date(this.selectSingleNode('@TStamp').text)</xsl:eval>
    <BR/>
    <xsl:eval>conv_trange(this.selectSingleNode('VALID/@TRange').text)</xsl:eval>
  </TD>
  <TD>
     <xsl:value-of select="VALID"/>
     <xsl:apply-templates select="PERIOD/*"/>
  </TD>
  </TR>
  </TABLE>
</xsl:template>

<xsl:template match="PERIOD/PREVAILING">
   <DIV>
   <xsl:attribute name="class">
       <xsl:eval>period_class(this.selectSingleNode('../@TRange').text)</xsl:eval>
   </xsl:attribute>
   <xsl:if test="../@Title"><xsl:value-of select="../@Title"/> </xsl:if>
   <xsl:value-of />
   </DIV>
</xsl:template>

<xsl:template match="PERIOD/VAR">
   <DIV class="var">
   <xsl:value-of select="@Title"/> <xsl:value-of />
   </DIV>
</xsl:template>

<xsl:template match="text()"><xsl:value-of /></xsl:template>

</xsl:stylesheet>
