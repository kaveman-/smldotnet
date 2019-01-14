<%@ Page language="c#" Codebehind="xq.aspx.cs" ValidateRequest="false" AutoEventWireup="true" Inherits="xq.WebForm1" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>XML Queries in SML.NET</title>
		<meta content="False" name="vs_showGrid">
		<meta content="Microsoft Visual Studio 7.0" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body bgColor="#ccffff" ms_positioning="GridLayout">
		<form id="Form1" method="post" runat="server">
			<P align="center"><FONT size="6">SML.NET Demo: XML Queries</FONT></P>
			</FONT>
			<P><asp:textbox id="TextBox1" style="Z-INDEX: 101; LEFT: 40px; POSITION: absolute; TOP: 246px" runat="server" Height="141px" Width="681px" TextMode="MultiLine"></asp:textbox></P>
			<P align="left"><asp:dropdownlist id="DropDownList1" style="Z-INDEX: 102; LEFT: 126px; POSITION: absolute; TOP: 220px" runat="server" AutoPostBack="True">
					<asp:ListItem Value="queries\q0.xq">q0.xq</asp:ListItem>
					<asp:ListItem Value="queries\q1.xq">q1.xq</asp:ListItem>
					<asp:ListItem Value="queries\q2.xq">q2.xq</asp:ListItem>
				</asp:dropdownlist><asp:label id="Label2" style="Z-INDEX: 103; LEFT: 42px; POSITION: absolute; TOP: 217px" runat="server" Font-Size="Large" Font-Names="Times New Roman" Height="20px" Width="80px">Query:</asp:label><asp:label id="Label1" style="Z-INDEX: 104; LEFT: 37px; POSITION: absolute; TOP: 75px" runat="server" Font-Size="Large" Font-Names="Times New Roman" Height="31px" Width="31px">Documents:</asp:label><asp:dropdownlist id="DropDownList2" style="Z-INDEX: 105; LEFT: 170px; POSITION: absolute; TOP: 79px" runat="server" AutoPostBack="True">
					<asp:ListItem Value="docs\bib.xml">bib.xml</asp:ListItem>
					<asp:ListItem Value="docs\reviews.xml">reviews.xml</asp:ListItem>
					<asp:ListItem Value="docs\prices.xml">prices.xml</asp:ListItem>
				</asp:dropdownlist><asp:textbox id="TextBox2" style="Z-INDEX: 106; LEFT: 39px; POSITION: absolute; TOP: 113px" runat="server" Height="89px" Width="679px" ReadOnly="True" TextMode="MultiLine"></asp:textbox><asp:textbox id="TextBox3" style="Z-INDEX: 107; LEFT: 40px; POSITION: absolute; TOP: 466px" runat="server" Height="153px" Width="683px" ReadOnly="True" TextMode="MultiLine"></asp:textbox><asp:button id="Button1" style="Z-INDEX: 108; LEFT: 265px; POSITION: absolute; TOP: 401px" runat="server" Font-Size="Large" Text="Evaluate!"></asp:button><asp:label id="Label3" style="Z-INDEX: 109; LEFT: 46px; POSITION: absolute; TOP: 430px" runat="server" Font-Size="Large" Font-Names="Times New Roman">Result:</asp:label><asp:label id="Label4" style="Z-INDEX: 110; LEFT: 213px; POSITION: absolute; TOP: 223px" runat="server">(this box is editable)</asp:label></P>
			<DIV style="Z-INDEX: 111; LEFT: 414px; WIDTH: 298px; POSITION: absolute; TOP: 52px; HEIGHT: 44px" ms_positioning="FlowLayout">
				<P>Loosely based on Fernandez, Simeon and Wadler, <EM><A href="http://www.research.avayalabs.com/user/wadler/papers/xalgebra-icdt/xalgebra-icdt.pdf">
							A Semistructured Monad for Semistructured Data</A></EM>, ICDT 2001.</P>
			</DIV>
		</form>
	</body>
</HTML>
