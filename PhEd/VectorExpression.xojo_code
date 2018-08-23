#tag Class
Protected Class VectorExpression
Inherits PhEd.Expression
	#tag Method, Flags = &h0
		Sub Value()
		  // This method returns the expression's vector value if all sub-expressions
		  // have well-defined values. Otherwise it raises an Undefined Value ExpressionError.
		  // Subclasses should override this method.
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		HasParentheses As Boolean
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="myText"
			Group="Behavior"
			Type="Text"
		#tag EndViewProperty
		#tag ViewProperty
			Name="myType"
			Group="Behavior"
			Type="PhEd.Expression.EType"
			EditorType="Enum"
			#tag EnumValues
				"0 - Literal"
				"1 - Variable"
				"2 - UnaryOperator"
				"3 - BinaryOperator"
			#tag EndEnumValues
		#tag EndViewProperty
		#tag ViewProperty
			Name="X"
			Group="Behavior"
			Type="integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Y"
			Group="Behavior"
			Type="integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="selected"
			Group="Behavior"
			Type="boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Width"
			Group="Behavior"
			Type="double"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Height"
			Group="Behavior"
			Type="double"
		#tag EndViewProperty
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
			Name="HasParentheses"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
