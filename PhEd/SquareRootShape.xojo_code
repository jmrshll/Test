#tag Class
Protected Class SquareRootShape
	#tag Method, Flags = &h0
		Sub Constructor(term as PhEd.Expression, g as Graphics)
		  // square root constructor
		  expr = term
		  height = expr.Height(g) + 5
		  width = expr.Width(g)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Draw(g as Graphics,X as integer, Y as integer)
		  // draw the square root 
		  dim spaceOffSet as double = 4
		  if expr.GetText <> "/" then
		    dim X1 as double = X 
		    dim X2 as double = X + 5
		    dim Y1 as Integer = Y-height/4
		    dim Y2 as integer = Y
		    g.DrawLine(X1,Y1,X2,Y2)
		    X1 = X + 5
		    X2 = X + 5 + spaceOffSet
		    Y1 = Y 
		    Y2 = Y - height -2
		    g.DrawLine(X1,Y1,X2,Y2)
		    g.DrawLine(X2,Y2,X2+width+2,Y2)
		    expr.Draw(g,X2+1,Y)
		  else
		    dim X1 as double = X 
		    dim X2 as double = X + 5
		    dim Y1 as Integer = Y-height/4 + height/2
		    dim Y2 as integer = Y  + height/2
		    g.DrawLine(X1,Y1,X2,Y2)
		    X1 = X + 5 
		    X2 = X + 5 + spaceOffSet
		    Y1 = Y  + height/2
		    Y2 = Y - height + height/2
		    g.DrawLine(X1,Y1,X2,Y2)
		    g.DrawLine(X2,Y2,X2+width+2,Y2)
		    expr.Draw(g,X2+1,Y)
		  end if
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getHeight() As integer
		  // get height
		  return height
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getWidth() As integer
		  // get width 
		  return width
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setHeight(newHeight as integer)
		  // set height
		  height = newHeight
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setWidth(newWidth as integer)
		  // set width 
		  width = newWidth
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		expr As PhEd.Expression
	#tag EndProperty

	#tag Property, Flags = &h0
		height As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		width As Integer
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="width"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="height"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
