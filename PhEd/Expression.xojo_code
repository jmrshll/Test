#tag Class
Protected Class Expression
	#tag Method, Flags = &h0
		Function BaselineOffset(g As Graphics) As Double
		  //A special routine needed for 
		  //formatting fractions on top of fractions.
		  //It is called from ScalarBinaryOperator.Draw
		  //when that draw method receives a / operator.
		  If (myType = Expression.Etype.BinaryOperator) And (Self.GetText = "/") Then
		    Return GetRightChild.Height(g)
		  Else
		    Return 0
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ClearParentheses()
		  //ClearParentheses sets the HasParentheses
		  //property of an expression, or of an item in an
		  //expression, to False.
		  HasParentheses = False
		  If myType = Expression.EType.BinaryOperator Then
		    GetLeftChild.ClearParentheses
		    GetRightChild.ClearParentheses
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub compute()
		  // This method computes the value of an Expression and 
		  // is one of the options of the menu allowing the user to solve equations
		  // To do that we break the problem of computing the value into different parts:
		  // Binary Operator: We need to determine if any of its expressions has a variable 
		  // If any of the sides has a variable, we do not compute the binary operator because that would negate the variable
		  // otherwise, we compute its value.
		  // Unary Operator: Similarly with the binary operator, if there is no variable compute its value 
		  
		  if getType = Expression.EType.BinaryOperator then 
		    if not GetLeftChild.hasVariable and not GetRightChild.hasVariable then 
		      myText = Value.ToText
		      myType = Expression.EType.Literal 
		      myLeftChild = nil 
		      myRightChild = nil
		    end if 
		  elseif getType = Expression.EType.UnaryOperator then 
		    if not GetLeftChild.hasVariable then 
		      myText = Value.ToText
		      myType = Expression.EType.Literal 
		      myLeftChild = nil 
		      myRightChild = nil
		    end if 
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  // empty constructor for error cases
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(value as Text)
		  // The Expression Constructor takes as a parameter the text of the node of the expression
		  // based on the text, its type can be one of the following:
		  // Number => Literal 
		  // Any numerical sign => Binary Operator 
		  // Any function => Unary Operator
		  // Any letter of the english alphabet => Variable 
		  // This new architecture of the constructor makes the classes of the program simpler 
		  // as we can combine the previously 4 more classes into this one. 
		  // Apart from simplicity, this change has technical merit because 
		  // it allows for directly changing children nodes given their parent
		  // allowing us to circumvent the need of reparsing the entire expression. 
		  // That was impossible with the previous tree stucture in which each node was a separate class
		  // because there was no way to directly change the contents of a child node. 
		  // Despite the seemingly large number of methods, in total,
		  // the number methods is the same as in the original version.
		  // The only difference is that all the methods, in this case,
		  // are contained in the same class for the reasons mentioned above.  
		  // Any changes in methods of the other classes that were replaced 
		  // could be directly copied to the methods of this class that correspond to them 
		  
		  if PhEd.isADigit(value) then 
		    myType = Expression.EType.Literal 
		  elseif PhEd.isAVariable(value) then 
		    myType = Expression.Etype.Variable
		    Defined = false 
		  elseif PhEd.IsValidOperator(value) then
		    myType = Expression.EType.BinaryOperator
		  elseif PhEd.isValidFunction(value) then 
		    myType = Expression.Etype.UnaryOperator
		    dim xs as XojoScript
		    xs = GetFunctionScript(value)
		    Script = xs
		    if xs = nil then
		      raise new ExpressionException("Undefined Function")
		    else
		      xs.Context = self // set this so that the Xojoscript can see theArgVal and theResult properties
		      if not xs.Precompile(XojoScript.OptimizationLevels.High) then // precompile the XojoScript
		        raise new ExpressionException("Function Precompile Failed")  // if it fails, raise an exception
		      else // sets the text property.
		        myText = value
		      end if
		    end if
		  end if
		  myText = value
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function copyNode() As PhEd.Expression
		  // This is a very useful method in reducing the amount of work done in other class.
		  // It copies the current node to a temporary node and returns the temporary node. 
		  // To begin with, we create a new temporary node with the text of the expression we are copying 
		  // Afterwards, we set the left and right child pointers of the new node to point 
		  // at copies of left and right child of this node. 
		  // Consequently, we have created a copy that we can use. 
		  dim tempNode as PhEd.Expression = new PhEd.Expression(getText)
		  tempNode.SetLeftChild(getLeftChild)
		  tempNode.SetRightChild(getRightChild)
		  return tempNode
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub decompose()
		  // This method applies Horner's Method to a number and decomposes it into powers of ten 
		  // Firstly, we check that we are applying this method to a number 
		  // Secondly, if that is the case, we call the main method decompose to this node
		  // Thirdly, we have broken down the number of this node into each sub-parts and store the sum of its sub parts
		  // For instance, if this method gets called to the number 154. the tree will become:
		  //                +
		  //             /    \
		  //         100    +
		  //                  /  \ 
		  //               50    4
		  
		  if getType = Expression.EType.Literal then 
		    dim d as PhEd.Expression = decompose(self)
		    setNode(d.GetText,d.GetLeftChild,d.GetRightChild)
		  end if 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DefineVariable(value as double)
		  // if the type of the node is a variable, then define it 
		  if myType = Expression.Etype.Variable then 
		    if defined = false then 
		      defined = true 
		      myValue = value 
		    end if 
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Draw(g as Graphics, X as double, Y as double)
		  // NOTE: The helper methods DrawScalar, DrawUnaryOperator, DrawBinaryOperator, DrawVariable 
		  // are almost identical with the respective Draw methods of the sub classes ScalarLiteral, UnaryOperator, 
		  // BinaryOperator, and Variable of the previous version of the program 
		  
		  // This method draws the expression in the Graphics object g with its center
		  // at the coordinates given by X and Y.
		  // Firstly, we simplify the expression and make the fractions look more presentable 
		  // Secondly, depending on the type of the expression, we call the appropriate method
		  
		  Simplify
		  fixFractions
		  if myType = Expression.Etype.Literal then 
		    // Draw the Scalar if the type of the expression is Literal
		    DrawScalar(g,X,Y)
		  elseif myType = Expression.EType.BinaryOperator then 
		    // Draw the Binary Operator if the type of the expression is Binary Operator
		    DrawBinaryOperator(g,X,Y)
		  elseif myType = Expression.EType.UnaryOperator then 
		    // Draw the Unary Operator if the type of the expression is Unary Operator
		    DrawUnaryOperator(g,X,Y)
		  elseif  myType = Expression.EType.Variable then 
		    // Draw the Variable if the type of the expression is Variable
		    DrawVariable(g,X,Y)
		  end if
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawBinaryOperator(g as Graphics, X as double, Y as double)
		  // NOTE: This method is almost identical to the Draw method in the BinaryOperator sub class of the previous version
		  // of the program. 
		  
		  //xAfterParens stores the value of where to start
		  //drawingthe left child if the scalar binary expression 
		  //is parenthesized.
		  Dim xAfterParens As Double = X
		  Dim lParen As New TextItemShape(g, "(", myScale)
		  Dim rParen As New TextItemShape(g, ")", myScale)
		  
		  //Below, the parentheses are drawn if
		  //the ScalarBinaryOperator expression
		  //HasParentheses property is True.  
		  //Remember that the HasParentheses
		  //property is set in the Expression.SetParentheses
		  //method which is called from the Paint event handler
		  //of FormattedExpression.                      
		  If HasParentheses Then
		    lParen.Draw(g, X, Y)  
		    xAfterParens = X + lParen.GetWidth
		    //Below, more space is allotted around fractions
		    //than around other items.
		    if GetText = "/" Then
		      xAfterParens = xAfterParens + lParen.GetWidth
		    End If
		    rParen.Draw(g,X + Width(g) - rParen.getWidth , Y)  
		    //Width(g) above would mean, for the input (1+2)*(3+4), a measure of (1+2)(3+4).
		    //That's why I use X and not xAfterParens.
		  End If
		  
		  //pointer stores the position where
		  //the next item should be drawn.
		  Dim pointer As Double = xAfterParens
		  
		  //Handle the different kinds of ScalarBinaryOperators
		  //with the Select Case statement. The variants are
		  //+, -, *, /, and ^.
		  Select Case GetText
		    
		  Case "+", "-"
		    GetLeftChild.Draw(g, pointer, Y)  //draw the left child
		    pointer = pointer + GetLeftChild.Width(g) //adjust the pointer
		    Dim space as new TextItemShape(g," ",myScale)
		    self.X = pointer + space.getWidth
		    self.Y = Y
		    Dim scalarLiteralTIS As New TextItemShape(g, myText, myScale)
		    Width =  scalarLiteralTIS.getWidth
		    Height = scalarLiteralTIS.getHeight
		    if selected = true then 
		      DrawSquare(g)
		    end if
		    Dim addOrSubtract As New TextItemShape(g, " " + getText + " ", myScale) //instantiate the operator
		    addOrSubtract.Draw(g, pointer, Y) //and draw it here
		    pointer = pointer + addOrSubtract.GetWidth //adjust pointer
		    GetRightChild.Draw(g, pointer, Y) //draw the right child
		  Case "*"
		    
		    // The special condition switches the order of two specific items
		    // It draws the literal first and then the variable. 
		    // The reason for this is so it can show expressions more clearly. 
		    // For instance, x(5) will be written 5x instead 
		    If GetRightChild.getType = Expression.Etype.Literal And GetLeftChild.getType =Expression.Etype.Variable Then
		      GetRightChild.Draw(g, pointer, Y)
		      dim rightChild as new TextItemShape(g,GetRightChild.GetText,GetRightChild.GetScale)
		      pointer = xAfterParens + rightChild.getWidth
		      self.X = pointer
		      self.Y = Y
		      Dim scalarLiteralTIS As New TextItemShape(g, myText, myScale)
		      self.Width =  scalarLiteralTIS.getWidth/2
		      self.Height = scalarLiteralTIS.getHeight
		      GetLeftChild.Draw(g, pointer, Y)
		    else
		      GetLeftChild.Draw(g, pointer, Y)
		      pointer = pointer + GetLeftChild.Width(g) 
		      self.X = pointer - rParen.getWidth
		      self.Y = myLeftChild.getY
		      Dim scalarLiteralTIS As New TextItemShape(g, "*", myScale)
		      self.Width =  scalarLiteralTIS.getWidth
		      self.Height = myLeftChild.Height
		      GetRightChild.Draw(g, xAfterParens + GetLeftChild.Width(g), Y)
		    End If
		    
		  Case "/"
		    
		    //We need to draw a fraction bar and set its
		    //length based on widerChildWidth. 
		    //widerChildWidth stores the value of the width
		    // of the numerator or the denominator,
		    //whichever is wider.
		    //tallerChild is necessary to position the denominator.
		    
		    Dim fractionBar As New CurveShape
		    Dim widerChildWidth As Double 
		    Dim tallerChildHeight As Double 
		    
		    //Determine which child is wider 
		    //and which is taller.
		    If GetLeftChild.Width(g) >= GetRightChild.Width(g) Then
		      widerChildWidth = GetLeftChild.Width(g)
		    Else
		      widerChildWidth = GetRightChild.Width(g)
		    End If
		    If GetLeftChild.Height(g) >= GetRightChild.Height(g) Then
		      tallerChildHeight = GetLeftChild.Height(g)
		    Else
		      tallerChildHeight = GetRightChild.Height(g)
		    End If
		    //Position the fraction bar
		    //in line with the crossbar of the + symbol.
		    fractionBar.X = pointer
		    fractionBar.X2 = pointer + widerChildWidth
		    fractionBar.Y   = Y - 3
		    fractionBar.Y2 = Y - 3
		    self.X = fractionBar.X
		    self.Y = fractionBar.Y+3
		    self.Width = widerChildWidth
		    self.Height = 6
		    
		    //centerOfFractionBar stores the value of the center
		    //of the fraction bar, which is necessary for positioning
		    //the numerator and denominator.
		    Dim centerOfFractionBar As Double = (fractionBar.X + fractionBar.X2)/2
		    
		    //To draw the
		    //numerator and denominator over and under
		    //the center of the fractionBar,
		    //we need the two variables declared below
		    //store the x-values where we should
		    //start drawing the left and right children,
		    //respectively.
		    Dim leftChildStartDraw As Double
		    Dim rightChildStartDraw As Double
		    leftChildStartDraw = centerOfFractionBar - 0.5*GetLeftChild.Width(g)  
		    rightChildStartDraw = centerOfFractionBar - 0.5*GetRightChild.Width(g)
		    
		    //Draw the numerator (left child) over the center of the fraction bar,
		    //with 3 pixels of space between it and fractionBar.
		    //Draw the denominator (right child) under the center of the fraction bar.
		    //The call to BaselineOffset is here to handle the case in which the input
		    //includes a fraction on top of a fraction. In that case, since the
		    //fraction bar is drawn with respect to the baseline
		    //rather than the height of the numerator or denominator,
		    //we use BaselineOffset to position the fraction bar.
		    
		    GetLeftChild.Draw(g, leftChildStartDraw, fractionBar.Y - 3 - GetLeftChild.BaselineOffset(g))
		    g.DrawObject(fractionBar)  
		    GetRightChild.Draw(g, rightChildStartDraw, fractionBar.Y + _
		    GetRightChild.Height(g) - GetRightChild.BaselineOffset(g))
		    
		  Case "^"
		    //Remember that we've already drawn the most external
		    //parentheses that surround the scalar binary expression.
		    //Remember xAfterParens records
		    //the width of the most external lParen.
		    GetLeftChild.Draw(g, pointer, Y)
		    GetRightChild.SetScale(0.7*myScale)
		    GetRightChild.Draw(g, xAfterParens + GetLeftChild.Width(g), Y - 0.6*GetLeftChild.Height(g))
		  Else 
		    
		    Raise New PhEd.ExpressionException("Undefined binary operator. " _
		    + "Encountered in ScalarBinaryOperator")
		    
		  End Select
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawScalar(g as Graphics, X as double, Y as double)
		  // NOTE: This method is almost identical to the Draw method in the ScalarLiteral sub class of the previous version
		  // of the program. 
		  
		  // Create the scalar text item
		  Dim scalarLiteralTIS As New TextItemShape(g, myText, myScale)
		  self.Width = scalarLiteralTIS.getWidth
		  self.Height = scalarLiteralTIS.getHeight
		  // check if there are any parenthesis and draw them
		  If HasParentheses Then
		    Dim lParen As New TextItemShape(g, "(", myScale)
		    Dim rParen As New TextItemShape(g, ")", myScale)
		    lParen.Draw(g, X, Y)
		    scalarLiteralTIS.Draw(g, X + lParen.GetWidth, Y)
		    self.X = X + lParen.GetWidth
		    self.Y = Y
		    rParen.Draw(g, X + lParen.GetWidth + scalarLiteralTIS.GetWidth, Y)
		  else
		    // otherwise simply draw the scalar
		    ScalarLiteralTIS.Draw(g, X, Y)
		    self.X = X 
		    self.Y = Y
		  End If
		  if selected = true then 
		    DrawSquare(g)
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawSquare(g as Graphics)
		  // This method draws a square around the selected item to make the user know 
		  // that the item was selected. 
		  // To do that, we follow these steps: 
		  // 1) Determine whether it is a drag or not. If the user is trying to drag the term, then it does not make sense 
		  //     to draw the square. We want to draw the square only when the user right clicks on the term
		  // 2) Depending on what type of node the user clicked, the program should draw different squares. 
		  //      For instance, in the case that the user right clicks on an operator, the program
		  //      should draw a rectangle which ranges through out all the expressions under the operator. 
		  //      Whereas, in the case of a literal, the program only needs to draw a square around the literal 
		  //      Therefore, we store the appropriate X,Y values of the start of the square along with the appropriate 
		  //      width and height 
		  // 3)  Finally, we draw the square from the Text Item Shape class given the previously stored information. 
		  if not WIndow1.isADrag then
		    Dim scalarLiteralTIS As New TextItemShape(g, myText, myScale)
		    if GetType = Expression.Etype.BinaryOperator then 
		      dim left as PhEd.Expression = getLeftMost
		      dim right as PhEd.Expression = getRightMost
		      dim width as double = right.Width(g)
		      scalarLiteralTIS.SetX(left.X)
		      scalarLiteralTIS.SetY(Y)
		      scalarLiteralTIS.myWidth = right.X - left.X + width
		    else
		      scalarLiteralTIS.SetX(X)
		      scalarLiteralTIS.SetY(Y)
		    end if
		    scalarLiteralTIS.drawSquare
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawUnaryOperator(g as Graphics, X as double, Y as double)
		  // NOTE: This method is almost identical to the Draw method in the UnaryOperator sub class of the previous version
		  // of the program. 
		  
		  Dim scalarUnOpTIS As New TextItemShape(g, GetText, myScale) //will hold information about the ScalarUnaryOperator
		  Dim lParen As New TextItemShape(g, "(", myScale)
		  Dim rParen As New TextItemShape(g, ")", myScale)
		  self.X = X
		  self.Y = Y
		  self.Height = lParen.getHeight
		  if selected then 
		    DrawSquare(g)
		  end if
		  //See the in-line comments below to 
		  //see what each line of code draws
		  //on the canvas, if the input is sin(x).
		  If HasParentheses Then
		    lParen.Draw(g, X, Y)  //   (
		    scalarUnOpTIS.Draw(g, X + lParen.GetWidth, Y)   //   (sin
		    //Parenthesize the scalar literal on which the operator operates.
		    lParen.Draw(g, X + lParen.GetWidth + scalarUnOpTIS.GetWidth, Y)  //   (sin(
		    GetLeftChild.Draw(g, X + lParen.GetWidth*2 + scalarUnOpTIS.GetWidth, Y)  //   (sin(x
		    rParen.Draw(g, X + lParen.GetWidth*2 + scalarUnOpTIS.GetWidth + GetLeftChild.Width(g), Y)   //   (sin(x)
		    rParen.Draw(g, X + lParen.GetWidth*2 + scalarUnOpTIS.GetWidth + GetLeftChild.Width(g) + _
		    rParen.GetWidth, Y)   //   (sin(x))
		  Else
		    if getText = "neg" then 
		      scalarUnOpTIS = new TextItemShape(g,"-",myScale)
		      scalarUnOpTIS.Draw(g, X, Y)   //   -
		      GetLeftChild.Draw(g, X + scalarUnOpTIS.GetWidth + lParen.GetWidth, Y)   //   - x
		    elseif getText = "abs" then 
		      scalarUnOpTIS = new TextItemShape(g,"|",myScale)
		      scalarUnOpTIS.Draw(g, X, Y)   //   |
		      GetLeftChild.Draw(g, X + scalarUnOpTIS.GetWidth, Y)   //   | x
		      scalarUnOpTIS.Draw(g,  X + scalarUnOpTIS.GetWidth + getLeftChild.Width(g), Y)   //   | x |
		    elseif getText = "sqrt" then 
		      dim sqrt as SquareRootShape = new SquareRootShape(GetLeftChild,g) 
		      sqrt.Draw(g,X,Y) // creates square root 
		    else 
		      scalarUnOpTIS.Draw(g, X, Y)   //   sin
		      //Parenthesize the scalar literal on which the operator operates.
		      lParen.Draw(g, X + scalarUnOpTIS.GetWidth, Y)   //   sin(
		      GetLeftChild.Draw(g, X + scalarUnOpTIS.GetWidth + lParen.GetWidth, Y)   //   sin(x
		      rParen.Draw(g, X + scalarUnOpTIS.GetWidth + lParen.GetWidth + GetLeftChild.Width(g), Y)   //   sin(x)
		    end if
		  End If
		  self.Width = Width(g) 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawVariable(g as Graphics, X as double, Y as double)
		  // NOTE: This method is almost identical to the Draw method in the Variable sub class of the previous version
		  // of the program. 
		  
		  // Create a scalarVariable Text Item Shape and draw it
		  Dim scalarVariableTIS As New TextItemShape(g, GetText, myScale)
		  scalarVariableTIS.Draw(g, X, Y)
		  self.Width = scalarVariableTIS.getWidth
		  self.Height = scalarVariableTIS.getHeight
		  self.X = X 
		  self.Y = Y
		  if selected = true then 
		    DrawSquare(g)
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub fixFractions()
		  // method to fix the fractions of an expression so they do not stack up
		  // ex: 2/3/4/5/6 should appear as:         2  
		  //                                                     ------------
		  //                                                      (3)(4)(5)(6)
		  
		  // To accomplish this, we do the following recursively: 
		  // 
		  // Check if we have two overlapping division operators and merge them
		  // by substituting one with a unifying one and then replacing the other with a multiplication node
		  // apply that to both the numerator and denominator
		  if myText = "/" and myLeftChild.myText = "/" then 
		    dim tempNode as phEd.Expression = CreateNode("*",GetLeftChild.GetRightChild,GetRightChild)
		    SetLeftChild(GetLeftChild.GetLeftChild) 
		    SetRightChild(tempNode)
		  elseif myText = "/" and myRightChild.myText = "/" then 
		    dim tempNode as phEd.Expression = CreateNode("*",GetLeftChild,GetRightChild.GetRightChild)
		    SetLeftChild(tempNode)
		    SetRightChild(GetRightChild.GetLeftChild) 
		  end if
		  if myLeftChild <> nil and myLeftChild.myText = "/"  and myText = "/" then
		     fixFractions
		  end if
		  if myLeftChild <> nil then 
		    myLeftChild.fixFractions 
		  end if 
		  if myRightChild <> nil then 
		    myRightChild.fixFractions
		  end if 
		  
		  moreFractions(self)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLeftChild() As PhEd.Expression
		  // return the left child
		  return myLeftChild
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getLeftMost() As PhEd.Expression
		  // get the leftmost element
		  if getType = Expression.EType.BinaryOperator or getType = Expression.Etype.UnaryOperator then 
		    return GetLeftChild.getLeftMost
		  else
		    return self
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetList() As PhEd.Expression()
		  // This method returns an ordered list of all of the subexpressions
		  // contained in an expression. The order is a breadth-first-traversal,
		  // (which is useful for constructing tree diagrams).
		  
		  dim myQueue(-1) as PhEd.Expression // queue of items
		  dim theItem as PhEd.Expression // this will hold the current item of interest
		  dim theList(-1) as PhEd.Expression // this will be the final list
		  myQueue.Append self // the calling object represents the first item to look at
		  while myQueue.Ubound >= 0 // we have remaining items to look at
		    theItem = myQueue(0) // get the first item in the queue
		    if not theItem.LeftIsEmpty then myQueue.Append theItem.myLeftChild // put the left child (if any) on the queue
		    if not theItem.RightIsEmpty then myQueue.Append theItem.myRightChild //and the right child (if any)
		    theList.Append theItem  // we have now duly examined this item
		    myQueue.Remove(0) // so remove it from the queue
		  wend
		  return theList // return the completed list
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetParent() As PhEd.Expression
		  // return my parent
		  return myParent
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetRightChild() As PhEd.Expression
		  // return the right child node
		  return myRightChild
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getRightMost() As PhEd.Expression
		  // get the rightmost element
		  if getType = Expression.EType.BinaryOperator then 
		    return GetRightChild.getRightMost
		  elseif getType = Expression.EType.UnaryOperator then 
		    return GetLeftChild.getRightMost
		  else
		    return self
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetScale() As double
		  // return the scale
		  return myScale
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetText() As Text
		  // return my text
		  return myText
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTreeHIndex() As Integer
		  // this recursive method gets a horizontal index
		  if myParent = nil then // this is the root expression
		    return 1 // which has an index of 1 by definition
		  else
		    if myParent.GetLeftChild = self then // if I am the left child of my parent, then
		      return myParent.GetTreeHIndex*2 - 1 // return this value calculated from my parent
		    else // I guess that I must be the right child
		      return myParent.GetTreeHIndex*2
		    end if
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTreeLevel() As integer
		  // This recursive method gets the level of the current expression on the tree diagram.
		  if myParent = nil then
		    return 0
		  else
		    return myParent.GetTreeLevel + 1
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getType() As PhEd.Expression.EType
		  return myType
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetUnits()
		  // This method reports the quantity's units.
		  // in the present version, the units are never set by any subclass,
		  // so myUnits will be nil. Interpret this as no units
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getX() As Integer
		  return X
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getY() As Integer
		  return Y
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function hasVariable() As Boolean
		  // determine if an expression has a variable 
		  if getType = Expression.EType.Literal then 
		    return false 
		  elseif getType = Expression.EType.Variable then 
		    return true 
		  elseif getType = Expression.Etype.UnaryOperator then 
		    return GetLeftChild.hasVariable
		  elseif getType = Expression.EType.BinaryOperator then 
		    return GetLeftChild.hasVariable or GetRightChild.hasVariable
		  end 
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Height(g as Graphics) As Double
		  // NOTE: This method is almost identical to the Height method of the sub classes of the previous version
		  // of the program. 
		  
		  // returns the height of an expression
		  if myType = Expression.Etype.Literal then 
		    // Overrides the superclass method.
		    Dim scalarLiteralTIS As New TextItemShape(g, GetText, myScale)
		    Return scalarLiteralTIS.GetHeight
		  elseif myType = Expression.Etype.BinaryOperator then 
		    // Overrides the superclass method. 
		    //expressionHeight is the height of 
		    //the expression, and we return
		    //its value at the end of this routine.
		    Dim expressionHeight As Double
		    //If we are dealing with a fraction, add the heights of of the
		    //numerator and the denominator and the spaces in between.
		    If GetText = "/" Then
		      // handle different cases for square root (does not work properly) 
		      if getLeftChild.getText = "sqrt" and GetRightChild.GetText = "sqrt" then 
		        expressionHeight = GetLeftChild.Height(g) + GetRightChild.Height(g) + 30
		      elseif GetLeftChild.GetText = "sqrt"  then 
		        expressionHeight = GetLeftChild.Height(g) + GetRightChild.Height(g) + 30
		      elseif GetRightChild.GetText = "sqrt" then 
		        expressionHeight = GetLeftChild.Height(g) + GetRightChild.Height(g) + 20
		      else // previous return for Expression height of fractions
		        expressionHeight = GetLeftChild.Height(g) + GetRightChild.Height(g) + 4 //4 pixels separate num. and denom.
		      end if
		      //If GetText = "^", remember that the right child
		      //is drawn at Y - 0.6*GetLeftChild.Height(g)
		      //(see ScalarBinaryOperator.Draw if you don't remember).
		    ElseIf GetText = "^" Then
		      expressionHeight = .6*GetLeftChild.Height(g) + GetRightChild.Height(g)
		    Else
		      expressionHeight = Max(GetLeftChild.Height(g), GetRightChild.Height(g))  //return the larger of the heights of the two children
		    End If
		    Return expressionHeight
		  elseif myType = Expression.EType.UnaryOperator then  // return the height of the binary operator
		    Dim scalarUnOpTIS As New TextItemShape(g, GetText, myScale)
		    if getText = "sqrt" then // return the height of the square root 
		      return GetLeftChild.Height(g) + 1
		    else
		      Return scalarUnOpTIS.GetHeight
		    end if
		  elseif myType = Expression.EType.Variable then  // return height of the variable 
		    Dim scalarVariableTIS As New TextItemShape(g, GetText, myScale)
		    Return scalarVariableTIS.GetHeight
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function inDenominator() As boolean
		  // method that determines if the selected expression is in the denominator 
		  if myParent = nil then 
		    return false 
		  elseif myParent.myText = "/" then 
		    return self = GetParent.GetRightChild
		  else 
		    return myParent.inDenominator
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function isDefined() As boolean
		  // determine if a variable has been defined
		  if getType = Expression.EType.BinaryOperator then 
		    return GetLeftChild.isDefined or GetRightChild.isDefined
		  elseif getType = Expression.EType.UnaryOperator then
		    return GetLeftChild.isDefined
		  elseif getType = Expression.EType.Literal then
		    return true 
		  elseif getType = Expression.EType.Variable then
		    return defined 
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function isLegal() As boolean
		  // determine if an expression is mathematically legal
		  if getText = "/" then // check if denominator is zero
		    if GetRightChild.isDefined and GetRightChild.Value = 0 then 
		      Window1.ExceptionType = 2
		      return false 
		    end if
		    return myLeftChild.isLegal and myRightChild.isLegal 
		  elseif  getText = "^" then // check if both exponent and base are zero
		    if GetRightChild.Value = 0 and GetLeftChild.Value = 0 and GetRightChild.isDefined and GetLeftChild.isDefined then // check if we raise zero to zero
		      Window1.ExceptionType = 3
		      return false 
		    end if
		    return myLeftChild.isLegal and myRightChild.isLegal 
		  elseif myType = Expression.Etype.BinaryOperator then 
		    return myLeftChild.isLegal and myRightChild.isLegal 
		  elseif myType = Expression.Etype.BinaryOperator then 
		    return myLeftChild.isLegal
		  else
		    return true
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function LeftIsEmpty() As Boolean
		  // check if the left child is empty
		  Return myLeftChild = nil
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MarkAsUndefined()
		  // mark a variable as undefined and make its value zerp
		  if myType = Expression.Etype.Variable then 
		    defined = false 
		    myValue = 0 
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function RightIsEmpty() As Boolean
		  // return true if the right child is empty
		  Return myRightChild = nil
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetChild(theChild as PhEd.Expression)
		  // set create new child and add it to an empty slot
		  if myType = Expression.EType.BinaryOperator then
		    if RightIsEmpty then
		      myRightChild = theChild
		      theChild.myParent = self
		    elseif LeftIsEmpty then
		      myLeftChild = theChild
		      theChild.myParent =self
		    else
		      raise new ExpressionException("Binary operator is already full")
		    end if
		  elseif myType = Expression.EType.UnaryOperator then
		    // note that the argument of a unary operator is the left child (right will remain empty)
		    if LeftIsEmpty then
		      myLeftChild = theChild
		      theChild.myParent = self
		    else
		      raise new ExpressionException("Unary operator is already full")
		    end if
		  else
		    raise new ExpressionException("Leaf expression types can't have children")
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetLeftChild(newLeft as PhEd.Expression)
		  // set left node
		  myLeftChild = newLeft 
		  if newLeft <> nil then newLeft.myParent = self
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setNode(newText as Text, newLeft as PhEd.Expression,  newRight as PhEd.Expression)
		  // set node to a different node 
		  // get new text, left, and right
		  myText = newText
		  myLeftChild = newLeft 
		  myRightChild = newRight
		  // set self as the parent of the new left and right
		  if newLeft <> nil then newLeft.myParent = self
		  if newRight <> nil then newRight.myParent = self
		  // based on the text, set the appropriate type
		  if PhEd.isADigit(myText) then 
		    myType = Expression.EType.Literal 
		  elseif PhEd.isAVariable(myText) then 
		    myType = Expression.Etype.Variable
		  elseif PhEd.IsValidOperator(myText) then
		    myType = Expression.EType.BinaryOperator
		  elseif PhEd.isValidFunction(myText) then 
		    myType = Expression.Etype.UnaryOperator
		  end if
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setParent(newParent as PhEd.Expression)
		  // set parent
		  myParent = newParent
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetParentheses()
		  //One call to SetParentheses sets the
		  // HasParentheses property of each item in the expression,
		  //working down from the top node of the expression tree.
		  If myType = Expression.EType.BinaryOperator Then
		    //Some HasParentheses property values depend on the parent.
		    If GetText = "*" Then
		      //do nothing. It is the children that will be parenthesized, not the whole binary expression.
		    ElseIf myParent <> Nil Then
		      
		      If GetParent.GetText = "*" Then
		        if getText = "^" then
		          
		        else 
		          HasParentheses = True
		        end if
		        //The case below is for if the input is something like (a+b)^3.
		        //Then we want the output to be to have a+b parenthesized,
		        //but the 3 not parenthesized.
		      ElseIf GetParent.GetText = "^" And (GetText = "+" Or GetText = "-") And Self = GetParent.GetLeftChild Then
		        HasParentheses = True
		        // added case when the user wants to input an expression like a - ( a + b )
		      Elseif getParent <> nil and self is GetParent.GetRightChild and getParent.getText = "-" and (getText = "+" or GetText  = "-") then
		        HasParentheses = True
		        // also added case in which the user want sot have a negation 
		      elseif getParent <> nil and getParent.getText = "neg" and self is GetParent.GetLeftChild and (getText = "-" or getText = "+") then 
		        HasParentheses = true 
		      end if
		    Else
		      ClearParentheses
		    End If
		    
		    GetLeftChild.SetParentheses
		    GetRightChild.SetParentheses
		    
		  ElseIf myType = Expression.EType.UnaryOperator Then
		    
		    GetLeftChild.SetParentheses
		    
		  ElseIf myType = Expression.EType.Literal then
		    
		    //For the input 2*2, we want the formatted expression
		    //to be (2)(2).
		    //If the input is 2*x, we want the formatted expression
		    //to be 2x.
		    If myParent <> Nil And GetParent.GetText = "*" And _ 
		      Not (GetParent.GetRightChild.hasVariable Or GetParent.GetLeftChild.hasVariable or GetParent.GetRightChild.getText = "sqrt" or GetParent.GetLeftChild.getText = "sqrt") Then  
		      HasParentheses = True
		    Else
		      ClearParentheses
		    End If
		    
		  End If
		  
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub setRightChild(newRight as PhEd.Expression)
		  // set the right child
		  myRightChild = newRight
		  if newRight <> nil then newRight.myParent = self
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetScale(myScaleFactor As Double)
		  // NOTE: This method is almost identical to the Draw method in the BinaryOperator sub class of the previous version
		  // of the program. 
		  
		  //With one call to SetScale, the myScale property
		  //for each item in the expression is set.
		  //SetScale is called from FormattedExpression.Paint.
		  //As of now, the only time the scale of items in the 
		  //expression is altered is if there is a ^ operator
		  //in the expression.
		  myScale = myScaleFactor
		  
		  If myType = Expression.EType.BinaryOperator Then
		    GetLeftChild.SetScale(myScaleFactor)
		    If GetText = "^" Then
		      GetRightChild.SetScale(0.8*myScale)
		      If myParent <> nil and GetParent.GetText = "^" Then
		        GetParent.myScale = myScale*0.8
		      End If
		    Else
		      GetRightChild.SetScale(myScaleFactor)
		    End If
		  ElseIf myType = Expression.EType.UnaryOperator then
		    GetLeftChild.SetScale(myScaleFactor)
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Simplify()
		  // This method simplifies the expression through the following process:
		  // Recursively look and replace the following with:
		  // 1) Replace addition and subtraction nodes that have 0 as one of their children with the other child 
		  // 2) Similarly, replace multiplication and division nodes that have 1 as one of their children
		  // 3) Replace multiplication or division nodes that have 0 as on of their children with 0 (
		  //     (for division only if it is on the numerator)
		  // 4) Cancel trigonometric functions that negate each other such as arcsin(sin(x)) = x
		  // 5) Cancel square roots with squares and substitute them with absolute values or with their argument 
		  //     based on their relative position in the tree structure 
		  
		  if myText = "*" then 
		    // if there is a product containing a 0, replace it with 0
		    if myLeftChild.myText = "0" or myRightChild.myText = "0"  then 
		      setNode("0",nil,nil)
		    elseif myRightChild.myText = "1" then // if it contains 1, replace it with the non 1 child
		      dim temp as Text = GetLeftChild.getText
		      setNode(temp,GetLeftChild.GetLeftChild,GetLeftChild.GetRightChild)
		    elseif myLeftChild.myText = "1" then
		      dim temp as Text = GetRightChild.getText
		      setNode(temp,GetRightChild.GetLeftChild,GetRightChild.GetRightChild)
		    end if
		  elseif myText = "+" then  
		    // if there is a sum containing a 0, replace it with the non 0 child
		    if myLeftChild.myText = "0" then 
		      dim temp as Text = GetRightChild.getText
		      setNode(temp,GetRightChild.GetLeftChild,GetRightChild.GetRightChild)
		    elseif myRightChild.myText = "0" then 
		      dim temp as Text = GetLeftChild.getText
		      setNode(temp,GetLeftChild.GetLeftChild,GetLeftChild.GetRightChild)
		    elseif myRightChild.myText = "neg" then 
		      setNode("-",myLeftChild,myRightChild.myLeftChild)
		    elseif myLeftChild.myText = "neg" then 
		      setNode("-",myRightChild,myLeftChild.myLeftChild)
		    end if
		  elseif myText = "/" then 
		    // if there is a division with a numberator that is 0, replace it with 0
		    if myLeftChild.myText = "0" then 
		      setNode("0",nil,nil)
		    elseif myRightChild.myText = "1" then // if its denominator is 1, replace it with the numerator
		      dim temp as Text = GetLeftChild.getText
		      setNode(temp,GetLeftChild.GetLeftChild,GetLeftChild.GetRightChild)
		    end if
		  elseif myText = "-" then 
		    // if there is a subtraction whose second term is 0, replace it with the non 0 child
		    if  myRightChild.myText = "0" then 
		      dim temp as Text = GetLeftChild.getText
		      setNode(temp,GetLeftChild.GetLeftChild,GetLeftChild.GetRightChild)
		    elseif myLeftChild.myText = "0" then 
		      setNode("neg",myRightChild,nil)
		    elseif myRightChild.myText = "neg" then 
		      setNode("+",myLeftChild,myRightChild.myLeftChild)
		    end if
		  elseif getText = "^" and getRightChild.getText = "2" and getLeftChild.getText = "sqrt" then  
		    setNode(GetLeftChild.GetLeftChild.GetText,GetLeftChild.GetLeftChild,GetLeftChild.GetLeftChild.GetRightChild)
		  elseif getText = "sqrt" and GetLeftChild.getText = "^" and GetLeftChild.GetRightChild.GetText = "2" then 
		    setNode("abs", GetLeftChild.GetLeftChild,nil)
		  elseif (getText = "arcsin" and GetLeftChild.GetText = "sin") or (getText = "sin" and GetLeftChild.GetText = "arcsin") or (getText = "arctan" and GetLeftChild.GetText = "tan") or (getText = "tan" and GetLeftChild.GetText = "arctan") or (getText = "cos" and GetLeftChild.GetText = "arccos") or  (getText = "arccos" and GetLeftChild.GetText = "cos") then 
		    setNode(GetLeftChild.GetLeftChild.GetText,GetLeftChild.GetLeftChild.GetLeftChild,getLeftChild.GetLeftChild.GetRightChild)
		  end if
		  // simplify both right and left child
		  if RightIsEmpty <> true then  myRightChild.Simplify
		  if LeftIsEmpty <> true then myLeftChild.Simplify
		  
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Value() As double
		  // This method returns the value of the expression
		  // To do this we recursively apply the operator of each node to each children 
		  // and return the value when we encounter leaf nodes
		  
		  if myType = Expression.Etype.BinaryOperator then // if it is a binary operator
		    dim LeftItem as PhEd.Expression = PhEd.Expression(self.GetLeftChild)
		    dim RightItem as PhEd.Expression = PhEd.Expression(self.GetRightChild)
		    // get the two children and based on the type, perform the appropriate operation
		    Select Case myText
		    Case "+"
		      return LeftItem.Value + RightItem.Value
		    Case "-"
		      return LeftItem.Value - RightItem.Value
		    Case "*"
		      return LeftItem.Value * RightItem.Value
		    Case "/"
		      return LeftItem.Value / RightItem.Value
		    Case "^"
		      return LeftItem.Value ^ RightItem.Value
		    End Select
		    // otherwise if we have a function, perform the function
		  elseif myType = Expression.EType.UnaryOperator then 
		    theArgVal = Expression(self.GetLeftChild).Value
		    Script.Run
		    return theResult // this is the result
		    // otherwise, return the value of the variable or the literal 
		  elseif myType = Expression.EType.Literal then 
		    return Val(myText) 
		  elseif myType = Expression.EType.Variable then 
		    return myValue
		  end if 
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Width(g as Graphics) As Double
		  // NOTE: This method is almost identical to the Draw method in the BinaryOperator sub class of the previous version
		  // of the program. 
		  
		  // return the total width of the expression
		  
		  if myType = Expression.Etype.Literal then 
		    // Overrides the superclass method.
		    Dim expressionLength As Double
		    
		    If HasParentheses Then
		      //Declare as TextItemShapes all of the items that will be drawn. 
		      Dim lParen As New TextItemShape(g, "(", myScale)
		      Dim scalarLiteralTIS As New TextItemShape(g, GetText, myScale)
		      Dim rParen As New TextItemShape(g, ")", myScale)
		      expressionLength =  lParen.GetWidth + scalarLiteralTIS.GetWidth + rParen.GetWidth
		    Else
		      Dim scalarLiteralTIS As New TextItemShape(g, GetText, myScale)
		      expressionLength = scalarLiteralTIS.GetWidth
		    End If
		    
		    Return expressionLength
		    
		  elseif myType = Expression.Etype.BinaryOperator then 
		    //expressionLength stores the value of the width of the expression
		    Dim expressionLength As Double
		    
		    //We set the width depending on if the scalar binary expression
		    //has parentheses or not, and depending on which ScalarBinaryOperator
		    //we are dealing with.
		    Dim lParen As New TextItemShape(g, "(", myScale)
		    Dim rParen As New TextItemShape(g, ")", myScale)
		    
		    //binOpTIS stores the binary operator as a TextItemShape.
		    //By default it has no text value, but this is replaced in the 
		    //case that the ScalarBinaryOperator is a + or -.
		    Dim binOpTIS As New TextItemShape(g, "", myScale)
		    
		    //Handle different scalar binary operators differently.
		    Select Case GetText
		      
		    Case "+"
		      if HasParentheses then
		        Dim binOpTIS2 As New TextItemShape(g, " " + GetText + " ", myScale)
		        binOpTIS = binOpTIS2 //this gives binOpTIS all the values of binOpTIS2
		        return getLeftChild.Width(g) + binOpTIS.getWidth + GetRightChild.Width(g) + lParen.getWidth + rParen.getWidth
		      else 
		        Dim binOpTIS2 As New TextItemShape(g, " " + GetText + " ", myScale)
		        binOpTIS = binOpTIS2 //this gives binOpTIS all the values of binOpTIS2
		        return getLeftChild.Width(g) + binOpTIS.getWidth + GetRightChild.Width(g)
		      end if
		    Case "-"
		      if HasParentheses then
		        Dim binOpTIS2 As New TextItemShape(g, " " + GetText + " ", myScale)
		        binOpTIS = binOpTIS2 //this gives binOpTIS all the values of binOpTIS2
		        return getLeftChild.Width(g) + binOpTIS.getWidth + GetRightChild.Width(g) + lParen.getWidth + rParen.getWidth
		      else 
		        Dim binOpTIS2 As New TextItemShape(g, " " + GetText + " ", myScale)
		        binOpTIS = binOpTIS2 //this gives binOpTIS all the values of binOpTIS2
		        return getLeftChild.Width(g) + binOpTIS.getWidth + GetRightChild.Width(g)
		      end if
		    Case "*" 
		      return GetLeftChild.Width(g) + GetRightChild.Width(g)
		    Case "/"
		      
		      //In this case we must make sure the width reflects the length of the
		      //numerator OR the denominator, not both, just whichever is larger.
		      //widerChildWidth stores the width of the numerator
		      //or denominator, whichever is wider.
		      Dim widerChildWidth As Double
		      
		      If GetLeftChild.Width(g) >= GetRightChild.Width(g) Then
		        widerChildWidth = GetLeftChild.Width(g)
		      Else
		        widerChildWidth = GetRightChild.Width(g)
		      End If
		      
		      If HasParentheses Then
		        expressionLength = widerChildWidth + 2*lParen.GetWidth + 2*rParen.GetWidth //we want extra space around fractions
		      Else
		        expressionLength = widerChildWidth
		      End If
		      
		    End Select
		    
		    //We don't want any of the following
		    //to apply in the case that GetText = "/".
		    If GetText <> "/" Then
		      If HasParentheses Then
		        expressionLength = lParen.GetWidth + rParen.GetWidth + GetLeftChild.Width(g) + GetRightChild.Width(g) _
		        + binOpTIS.GetWidth
		      Else
		        expressionLength = binOpTIS.GetWidth + GetLeftChild.Width(g) + GetRightChild.Width(g)
		      End If
		    End If
		    
		    Return expressionLength
		  elseif myType = Expression.EType.UnaryOperator then 
		    // Overrides the superclass method.
		    Dim expressionLength As Double
		    
		    //The items to be drawn
		    Dim scalarUnOpTIS As New TextItemShape(g, GetText, myScale)
		    Dim lParen As New TextItemShape(g, "(", myScale)
		    Dim rParen As New TextItemShape(g, ")", myScale)
		    
		    If HasParentheses Then
		      if getText = "neg" then // ex: width of -(x-4) expression
		        scalarUnOpTIS = new TextItemShape(g,"-",myScale)
		        dim scalarTOpTIS as TextItemShape = new TextItemShape(g," ",myScale)
		        expressionLength = scalarUnOpTIS.GetWidth + scalarTOpTIS.getWidth + lParen.getWidth + rParen.getWidth + GetLeftChild.Width(g)
		      elseif  getText = "abs" then // ex: width of |x-4| expression
		        scalarUnOpTIS = new TextItemShape(g,"|",myScale)
		        expressionLength = 2*scalarUnOpTIS.GetWidth + GetLeftChild.Width(g)
		      elseif getText = "sqrt" then // ex: width of sqrt(x-4) Expression
		        expressionLength = 9 + GetLeftChild.Width(g) 
		      else // otherwise width of function
		        expressionLength =  lParen.GetWidth*2 + scalarUnOpTIS.GetWidth + _
		        GetLeftChild.Width(g) + rParen.GetWidth*2  // e.g. (sin(x))
		      end if
		    Else // width of function without parenthesis 
		      if getText = "neg" then // ex: - 5 
		        scalarUnOpTIS = new TextItemShape(g,"-",myScale)
		        dim scalarTOpTIS as TextItemShape = new TextItemShape(g," ",myScale)
		        expressionLength = scalarUnOpTIS.GetWidth + scalarTOpTIS.getWidth + GetLeftChild.Width(g)
		      elseif GetText = "abs" then // ex: |5|
		        scalarUnOpTIS = new TextItemShape(g,"|",myScale)
		        expressionLength = 2*scalarUnOpTIS.GetWidth + GetLeftChild.Width(g)
		      elseif getText="sqrt" then // ex: sqrt(3)
		        expressionLength = 9 + GetLeftChild.Width(g) 
		      else
		        expressionLength = scalarUnOpTIS.GetWidth + lParen.GetWidth + _
		        GetLeftChild.Width(g) + rParen.GetWidth  // e.g. sin(x)
		      end if
		    End If
		    Return expressionLength 
		  elseif myType = Expression.EType.Variable then 
		    // Overrides the superclass method.
		    Dim expressionLength As Double
		    
		    If HasParentheses Then
		      //Declare as TextItemShapes all of the items that will be drawn.
		      Dim lParen As New TextItemShape(g, "(", myScale)
		      Dim scalarVariableTIS As New TextItemShape(g, GetText, myScale)
		      Dim rParen As New TextItemShape(g, ")", myScale)
		      expressionLength =  lParen.GetWidth + scalarVariableTIS.GetWidth + rParen.GetWidth
		    Else
		      Dim scalarVariableTIS As New TextItemShape(g, GetText, myScale)
		      expressionLength = scalarVariableTIS.GetWidth
		    End If
		    Return expressionLength
		  end if
		End Function
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected Defined As boolean = true
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//Overwritten by subclasses
			
		#tag EndNote
		Protected HasParentheses As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		Height As double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private marked As boolean = false
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected myLeftChild As PhEd.Expression
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected myList() As PhEd.Expression
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected myParent As PhEd.Expression
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected myRightChild As PhEd.Expression
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected myScale As double = 1.0
	#tag EndProperty

	#tag Property, Flags = &h0
		myText As Text
	#tag EndProperty

	#tag Property, Flags = &h0
		myType As PhEd.Expression.EType
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected myValue As double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Script As XojoScript
	#tag EndProperty

	#tag Property, Flags = &h0
		selected As boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private theArgVal As double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private theResult As double
	#tag EndProperty

	#tag Property, Flags = &h0
		Width As double
	#tag EndProperty

	#tag Property, Flags = &h0
		X As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Y As integer
	#tag EndProperty


	#tag Enum, Name = EType, Type = Integer, Flags = &h0
		Literal
		  Variable
		  UnaryOperator
		BinaryOperator
	#tag EndEnum


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
	#tag EndViewBehavior
End Class
#tag EndClass
