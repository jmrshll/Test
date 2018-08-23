#tag Class
Protected Class Equation
	#tag Method, Flags = &h0
		Sub additionMover(moved as PhEd.Expression, expr as PhEd.Expression)
		  // helper method for when we are trying to move a term from one side to the other
		  // create a main subtraction node on the other side of the equation and 
		  // and append the moved element 
		  dim copyNode as PhEd.Expression = expr.copyNode
		  expr.setNode("-",copyNode,moved)
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub applyFunction(funct as Text)
		  // This method applies a function to both sides of the equation 
		  // It takes as input the String that represents the function 
		  // Then It replaces the leading node of both sides of the equation with that function
		  dim newLeft as PhEd.Expression = CreateNode(funct,LeftExpression,nil)
		  dim newRight as PhEd.Expression = CreateNode(funct,RightExpression,nil)
		  SetLeft(newLeft)
		  SetRight(newRight)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function areEqual() As boolean
		  // The method Value determines the value of the expression
		  // In this method, we use Value to check if the two sides of the equation have the same value 
		  // if not the equation is invalid and we update the ExceptionType 
		  if LeftExpression.Value <> RightExpression.Value then Window1.ExceptionType = 5
		  return LeftExpression.Value = RightExpression.Value
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ClearParentheses()
		  // Clear the parenthesis of both expressions
		  LeftExpression.ClearParentheses 
		  RightExpression.ClearParentheses
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub computeSides()
		  // Computes the result of each expression
		  LeftExpression.compute
		  RightExpression.compute
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  // constructor of the equation
		  // The equation class structure can be thought of as a class that has two main properties, the two expressions which 
		  // are on the two sides of the equation 
		  // When drawing the equation, we draw the first expression then the equal sign and then the second updating the 
		  // X and Y coordinates appropriately
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub decompose()
		  // This method aplies Horner's Method to break a number into each parts 
		  // For example, if the user attempts to decompose the number 354 then 
		  // the number will break to "300 + 50 + 4" 
		  // To do this, we get the left and right sides of the equation 
		  // and check if any term was selected to be decomposed 
		  // If that is the case, then we call the method decompose to that term
		  dim left as PhEd.Expression = Equation.GetLeft
		  dim right as PhEd.Expression = Equation.GetRight 
		  dim leftList() as PhEd.Expression = left.getList 
		  dim rightList() as PhEd.Expression = right.getList 
		  for i as integer = 0 to leftList.Ubound 
		    if leftList(i).selected = true then leftList(i).decompose
		  next 
		  for i as integer = 0 to rightList.Ubound 
		    if rightList(i).selected = true then rightList(i).decompose
		  next 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub divisionMover(moved as PhEd.Expression, expr as PhEd.Expression)
		  // helper method for when we are trying to move a term from one side to the other
		  // if it is the numerator then create a division main node on the other side
		  // then append the moved node as the denominator 
		  if Window1.isLeft then 
		    if expr.getText = "/" then 
		      dim copyNode as PhEd.Expression = expr.GetRightChild.copyNode
		      expr.GetRightChild.setNode("*",moved,copyNode)
		    else 
		      dim copyNode as PhEd.Expression = expr.copyNode
		      expr.setNode("/",copyNode,moved)
		    end if
		  else 
		    // otherwise create a multiplication main node on the other side
		    // then append the moved node 
		    if expr.getText = "/" then 
		      dim tempNode as PhEd.Expression = createNode("*", moved, expr.GetLeftChild)
		      expr.SetLeftChild(tempNode)
		    else 
		      dim copyNode as PhEd.Expression = expr.copyNode
		      expr.setNode("*",moved,copyNode)
		    end if
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Draw(g as Graphics, X as integer, Y as integer)
		  // Before drawing the equation ,simplfy the equation to make it more presentable
		  // This method draws the equation into g, with its center at coordinates X and Y.
		  // This method draws the left side of the equation first, updates the X and Y pointers
		  // then draws the equal sign 
		  // updates again the X and Y pointers 
		  // then draws the right side of the equation
		  Simplify
		  fixFractions
		  LeftExpression.Draw(g, X, Y)  //draw the left side
		  dim pointer as double = LeftExpression.Width(g) //adjust the pointer
		  dim equalSign as TextItemShape = new TextItemShape(g," = ",myScale)
		  dim spaceBar as TextItemShape = new TextItemShape(g," ",myScale)
		  Window1.equalSign_X = pointer + equalSign.getWidth
		  EqualSign.Draw(g, X + pointer, Y) //and draw it here
		  myX =  X + pointer + spaceBar.getWidth
		  myY = Y 
		  myWidth = EqualSign.GetWidth
		  myHeight = EqualSign.getHeight 
		  if selected = true then 
		    DrawSquare(g)
		  end if
		  pointer = pointer + EqualSign.GetWidth //adjust pointer
		  RightExpression.Draw(g, X + pointer, Y) //draw the right side
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawSquare(g as Graphics)
		  // Draw a square around the equal sign if selected
		  // This indicates that the user wants to insert a function or a term to both sides 
		  // After draing the square a menu will show up asking the user what they want to insert in both sides
		  if selected then
		    Dim scalarLiteralTIS As New TextItemShape(g, "=", myScale)
		    scalarLiteralTIS.SetX(myX)
		    scalarLiteralTIS.SetY(myY)
		    scalarLiteralTIS.drawSquare
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub findSelected(list() as PhEd.Expression, expr as PhEd.Expression)
		  // This method finds the selected term and moves it to the other side of the equation 
		  // To do this we go through the following process:
		  // Firstly, we determine if it is a series of additions/subtractions or a series of products/divisions
		  // we do this because, we need to determine if it is legal algebraically 
		  // for the user can move the term.
		  // For instance, if we did not do that, the user would be able to perform illegal moves of terms like:
		  //    5x + 3 = 15 
		  // The user otherwise would be able to move the 5 to the other side and transform the equation into:
		  //    x + 3 = 15 - 5 which is not mathematically correct 
		  // Secondly, we determine if the selected term is on the right or left of an operator.
		  // This is important because, in the case of subtraction and division,
		  // moving the left or right term of the operator should yield different results.
		  // If we did not do that, then we would again get incorrect results like in the following example:
		  //     5 - x = 3 
		  // moving the left or right term the operator should have different effects on the right side of the equation 
		  // Thirdly, now that we know the relative position of selected term, we need to copy that term to a temporary node
		  // Fourthly, after copying that term, we need to check the operator type of its parent 
		  // Finally, after we have the type of operator, we set the node of the selected term to 0, and pass the copied
		  // node to a helper function that moves the term based on which operator the term had as a parent.
		  // When all this is done we simplify both sides of the equation, to make the equation more presentable. 
		  dim temp as phEd.Expression 
		  // determine if we are dealing with a series of additions/subtractions or multiplications/divisions 
		  if list(0).getText = "+" or list(0).getText = "-" then 
		    Window1.isAddition = true
		  elseif list(0).getText = "*" or list(0).getText = "/" then 
		    Window1.isMultiplication = true
		  end if
		  // determine if the moved term is on the left, because if it is, in the case of subtraction and division
		  // the result will be different 
		  for j as integer = 0 to list.Ubound
		    if list(j).GetLeftChild <> nil and list(j).GetLeftChild.selected = true then 
		      Window1.isLeft = true 
		      exit
		    end if
		  next
		  // after all is done, we know how to move the selected term and just copy it 
		  // and call the appropriate helper method 
		  for i as integer = 0 to list.Ubound
		    if list(i).selected = true then 
		      temp = list(i).copyNode
		      // Edge Case:
		      // If the selected term does not have a parent, 
		      // then we know that we need to subtract it from the opposite side to move it.
		      // So we follow the same process as 
		      // if it is the child of an addition binary operator.
		      // Therefore, we need to only call the helper method for addition
		      if list(i).getParent = nil then 
		        list(i).setNode("0",nil,nil)
		        additionMover(temp,expr)
		        // if the parent binary operator is an addition binary operator, 
		        // and we know that we are dealing with a series of additons/subtractions
		        // we call the helper method for addition. 
		        // Otherwise, if we are not dealing with a series of additions/subtractions, 
		        // it would not be correct to move the term. 
		      elseif list(i).getParent.getText = "+"  then
		        if Window1.isAddition then 
		          list(i).setNode("0",nil,nil)
		          additionMover(temp,expr)
		        end if
		        // similarly for subtraction but instead call the subtraction helper method 
		      elseif list(i).getParent.getText = "-" then
		        if Window1.isAddition then 
		          list(i).setNode("0",nil,nil)
		          subtractionMover(temp,expr)
		        end if
		        // similarly for multiplication but instead we call the multiplication helper method 
		        // and also we need to make sure that we are dealing with a series of multiplications 
		      elseif list(i).getParent.getText = "*" then
		        if Window1.isMultiplication then 
		          list(i).setNode("1",nil,nil)
		          multiplicationMover(temp,expr,list(i))
		        end if
		        // similarly for division but instead we call the division helper method
		      elseif list(i).getParent.getText = "/" then
		        if Window1.isMultiplication then 
		          list(i).setNode("1",nil,nil)
		          divisionMover(temp,expr)
		        end if
		      end if
		    end if
		  next
		  // Finally, we simplify the two sides of the equation 
		  LeftExpression.Simplify
		  RightExpression.Simplify
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub fixFractions()
		  // This method calls fix fractions to both sides 
		  // The reason for this is because otherwise the program 
		  // shows functions in an unconventional way: 
		  //       1
		  //     -----
		  //       2
		  //     -----
		  //       3
		  // Rather, with this method, the program will show functions in a more logical way: 
		  //       1
		  //     -----
		  //     (2)(3)
		  LeftExpression.fixFractions
		  RightExpression.fixFractions
		  moreFractions(LeftExpression)
		  moreFractions(RightExpression)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLeft() As PhEd.Expression
		  // return the left expression
		  return LeftExpression
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetRight() As PhEd.Expression
		  // return the right expression
		  return RightExpression
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getScale() As double
		  // return the scale
		  return myScale
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function hasVariable() As Boolean
		  // determine if the equation has any variables
		  return GetLeft.hasVariable or GetRight.hasVariable
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Height(g as Graphics) As double
		  // This method returns the maximum height of the equation
		  // To find the maximum height, we compare the heights of each expression and return the highest of the two
		  if LeftExpression.Height(g) > RightExpression.Height(g) then
		     theHeight = LeftExpression.Height(g)
		  else 
		    theHeight = RightExpression.Height(g)
		  end if
		  return theHeight
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Insert2Both(t as Text, expr as PhEd.Expression)
		  // This method inserts a new term in both sides of the equation 
		  // For examply. the user can determine to multiply both sides 
		  // of an equation by 10
		  // that would transform the equation from:
		  //    x
		  //  ----   =   3
		  //  10 
		  // to: x = (3)(10)
		  // then the user can click on the equal sign and select "compute"
		  // from the menu which would give him/her x = 30 as the answer 
		  // The way this happens is by setting creating two new nodes. which 
		  // are the newLeft and the newRight and 
		  // replacing the old sides with these nodes 
		  dim newLeft as PhEd.Expression = CreateNode(t,getLeft,expr) 
		  SetLeft(newLeft)
		  dim newRight as PhEd.Expression = CreateNode(t,getRIght,expr) 
		  SetRight(newRight)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function isLegal() As boolean
		  // determine whether the equation is legal 
		  // if it has a variable then ww do not have to determine if the values of the two sides are equal, because we are solving
		  // in terms of the variable.
		  // Instead, if the equation has a variable, we check if the two expressions are legal
		  if hasVariable then
		    return LeftExpression.isLegal and RightExpression.isLegal
		  else 
		    // If the equation does not have a variable, then logically, because we have an equality, the two sides 
		    // have to also be equal 
		    return LeftExpression.isLegal and RightExpression.isLegal and areEqual
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub moveSelected()
		  // This method is called by the canvas event handlers to move the selected item 
		  // from one side of the equation to the other
		  // To do this we follow the following process: 
		  // Firstly, we get the lists of the two expressions that correspond to each side of the Equation
		  // Then, pass those two lists to the findSelected method 
		  // the finds the selected term and depending on the parent's operator, 
		  // moves it to the other side 
		  dim left as PhEd.Expression = Equation.GetLeft
		  dim right as PhEd.Expression = Equation.GetRight 
		  dim leftList() as PhEd.Expression = left.getList 
		  dim rightList() as PhEd.Expression = right.getList 
		  Window1.isLeft = false
		  findSelected(leftList, RightExpression)
		  findSelected(rightList,LeftExpression)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub multiplicationMover(moved as PhEd.Expression, otherSide as PhEd.Expression, current as PhEd.Expression)
		  // create a division main node on the other side
		  // then append the moved node as the denominator 
		  dim copyNode as PhEd.Expression = otherSide.copyNode
		  if current.inDenominator then
		    otherSide.setNode("*",moved,copyNode)
		  else
		    otherSide.setNode("/",copyNode,moved)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetExpressions(Left as PhEd.Expression, Right as PhEd.Expression)
		  // This method set the left and right expressions.
		  // Checks for compatible type and units need to be implemented.
		  // Should raise appropriate errors if not correct. Otherwise:
		  LeftExpression = Left
		  RightExpression = Right
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetLeft(newLeft as PhEd.Expression)
		  // Set LeftExpression 
		  LeftExpression = newLeft
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetParentheses()
		  // Set Parentheses to both the left and right expressions
		  LeftExpression.SetParentheses
		  RightExpression.SetParentheses
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetRight(newRight as PhEd.Expression)
		  // set the right side
		  RightExpression = newRight
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setScale(newScale as double)
		  // update myScale and set the scale of both the right and the left expressions
		  myScale = newScale
		  LeftExpression.SetScale(myScale) 
		  RightExpression.SetScale(myScale)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Simplify()
		  // simplify both sides
		  LeftExpression.Simplify
		  RightExpression.Simplify
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub subtractionMover(moved as PhEd.Expression, expr as PhEd.Expression)
		  // helper method for when we are trying to move a term from one side to the other
		  // create a copy of the other side's main node
		  dim copyNode as PhEd.Expression = expr.copyNode
		  // if it is the left side of the subtraction
		  if Window1.isLeft then 
		    // create a main subtraction node on the other side of the equation and append the moved element
		    expr.setNode("-",copyNode,moved)
		  else 
		    //otherwise, add the subtracted term to the other side
		    if expr.getText = "neg" then 
		      expr.setNode("-",moved,expr.GetLeftChild)
		    else 
		      expr.setNode("+",copyNode,moved)
		    end if
		  end if
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Width(g as Graphics) As Double
		  // return the total width of the equation: LeftExpression.Width + equalSign.Width + RightExpression.Width
		  dim equalSign as TextItemShape = new TextItemShape(g,"=",myScale)
		  theWidth = LeftExpression.Width(g) + EqualSign.getWidth + RightExpression.Width(g)
		  return theWidth
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private LeftExpression As PhEd.Expression
	#tag EndProperty

	#tag Property, Flags = &h0
		myHeight As double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private myScale As double = 1.0
	#tag EndProperty

	#tag Property, Flags = &h0
		myWidth As double
	#tag EndProperty

	#tag Property, Flags = &h0
		myX As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		myY As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private RightExpression As PhEd.Expression
	#tag EndProperty

	#tag Property, Flags = &h0
		selected As boolean = false
	#tag EndProperty

	#tag Property, Flags = &h21
		Private theHeight As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private theWidth As Integer
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
			Name="selected"
			Group="Behavior"
			InitialValue="false"
			Type="boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="myX"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="myY"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="myWidth"
			Group="Behavior"
			Type="double"
		#tag EndViewProperty
		#tag ViewProperty
			Name="myHeight"
			Group="Behavior"
			Type="double"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
