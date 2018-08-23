#tag Module
Protected Module PhEd
	#tag Method, Flags = &h0
		Sub checkMargins()
		  // This method helps ensure that the values remain inside the margins of the canvas
		  // To do this, we get the outOfBounds property of the canvas and rescale and refresh until the expression 
		  // length can fit within the bounds of the canvas 
		  
		  if Window1.outOfBounds then // if it is out of bounds, rescale and refresh
		    Window1.scaleFactor = Window1.scaleFactor * 0.8
		    Window1.FormattedExpression.Refresh
		  elseif Window1.inBounds and Window1.scaleFactor < 1/0.8 then // if it is too small, rescale and refresh
		    Window1.scaleFactor = Window1.scaleFactor * 1/0.8
		    Window1.FormattedExpression.Refresh
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub clearList(List() as PhEd.Expression, current as PhEd.Expression)
		  // make sure no term is selected except from the current term 
		  for i as integer = 0 to list.ubound 
		    if list(i) <> current then
		      list(i).selected = false
		    end if
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function containsClick(X as integer, Y as integer, expr as phEd.Expression) As boolean
		  // determine if a term was clicked within its borders
		  return expr.getX < X and expr.getX+expr.Width > X and expr.getY > Y and expr.getY-expr.Height < Y
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function CreateNode(text as Text, left as PhEd.Expression, right as PhEd.Expression) As PhEd.Expression
		  // create a node based on the parameters past
		  dim tNode as PhEd.Expression = new PhEd.Expression(text)
		  tNode.SetLeftChild(left)
		  tNode.setRightChild(right)
		  return tNode
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function createRepresentation(ar() as PhEd.Expression) As PhEd.Expression
		  // pre condition: the array past has an even number of elements
		  // This is the helper method for the decompose method
		  dim pairArr() as PhEd.Expression  
		  while ar.Ubound >= 0 
		    dim tempNode as PhEd.Expression = new PhEd.Expression("+")
		    dim d1 as PhEd.Expression = ar.Pop
		    dim d2 as PhEd.Expression = ar.Pop
		    if d1.Value > d2.Value then 
		      tempNode.SetLeftChild(d1)
		      tempNode.setRightChild(d2)
		    else 
		      tempNode.SetLeftChild(d2)
		      tempNode.setRightChild(d1)
		    end if
		    pairArr.Append(tempNode)
		  wend 
		  dim size as integer = pairArr.Ubound + 1
		  if size = 1 then 
		    return pairArr.Pop
		  elseif size mod 2 = 1 then
		    dim plusNode as PhEd.Expression = new PhEd.Expression("+")
		    plusNode.setRightChild(pairArr.pop)
		    plusNode.setLeftChild(createRepresentation(pairArr))
		    return plusNode
		  else 
		    return createRepresentation(pairArr)
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function decompose(expr as PhEd.Expression) As PhEd.Expression
		  // pre condition: the expression is a literal 
		  // Use horner's method to create an arrays out of the 
		  // the coefficients of the powers of ten that can represent the number.
		  // Afterwards, recursively create an expression of additions that can represent that number 
		  // through the helper method createRepresentation
		  
		  dim amount as Double = expr.Value 
		  dim exprAr() as PhEd.Expression
		  dim placeholder as Integer = 0 
		  dim startMod as Integer = 10 
		  while amount > 0 
		    placeholder = amount mod startMod 
		    amount = amount - placeholder 
		    dim temp as PhEd.Expression = new PhEd.Expression(placeholder.ToText)
		    exprAr.Append(temp)
		    startMod = startMod*10 
		  wend 
		  dim size as integer = exprAr.Ubound + 1
		  dim resultNode as PhEd.Expression
		  if size = 1 then 
		    resultNode = expr
		  elseif size mod 2 = 1 then 
		    resultNode = new PhEd.Expression("+")
		    resultNode.SetLeftChild(exprAr.Pop)
		    dim rightChild as PhEd.Expression = createRepresentation(exprAr)
		    resultNode.setRightChild(rightChild)
		  else 
		    resultNode = createRepresentation(exprAr)
		  end if
		  return resultNode
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function digitOrVar(txt as Text) As boolean
		  // determine if a term is a digit or a variable
		  return isAVariable(txt) or IsADigit(txt)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawException()
		  // Based on the type of exception, handle it by showing the proper error message
		  if WIndow1.ExceptionType = 0 then
		    //g.DrawString("Error: Parenthesis Mismatch", 5, g.Height/4) 
		    Window1.TextArea1.setString("Error: Parenthesis Mismatch")
		  elseif WIndow1.ExceptionType = 1 then
		    //g.DrawString("Error: Illegal Character", 5, g.Height/4) 
		    Window1.TextArea1.setString("Error: Illegal Character")
		  elseif WIndow1.ExceptionType = 2 then
		    //g.DrawString("Error: Can't divide by zero", 5, g.Height/4) 
		    Window1.TextArea1.setString("Error: Can't divide by zero")
		  elseif WIndow1.ExceptionType = 3 then
		    //g.DrawString("Error: Can't raise zero to the power of zero", 5, g.Height/4) 
		    Window1.TextArea1.setString("Error: Can't raise zero to the power of zero")
		  elseif WIndow1.ExceptionType = 4 then
		    //g.DrawString("Error: Invalid Mathematical Expression", 5, g.Height/4) 
		    Window1.TextArea1.setString("Error: Invalid Mathematical Expression")
		  elseif WIndow1.ExceptionType = -1 then
		    //g.DrawString("Error: Invalid Mathematical Expression", 5, g.Height/4) 
		    Window1.TextArea1.setString("Empty Text Field")
		  elseif Window1.ExceptionType = 5 then 
		    Window1.TextArea1.setString("Error: Invalid Equation")
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawTree(g as Graphics, theExpression as PhEd.Expression)
		  // This method draws the tree implied by the supplied expression
		  // This is based on an algorithm discussed at
		  // http://stackoverflow.com/questions/14184655/set-position-for-drawing-binary-tree
		  
		  dim HSpace as integer = 50 // this will be the width of an item
		  dim VSpace as integer = 15 // this will be the line height of a level
		  dim VOffset as integer = 15 // offset from the top
		  dim HOffset as integer = 25 // offset from the left
		  dim theList(-1) as PhEd.Expression // this will be an ordered list of items in the expression
		  dim theLevelList(-1) as integer // this will be a parallel list of levels
		  dim theHIndexList(-1) as integer // this will be a parallel list of indices
		  dim theShape as StringShape // this will be the particular shape in question
		  dim theItem as PhEd.Expression // this will be the particular item we are looking at
		  dim LastLevel as integer // this will be the value of the maximum level we reach
		  theList = theExpression.GetList // get the list of expressions
		  for i as integer = 0 to theList.Ubound // scan through the list and create the parallel lists
		    theItem = theList(i) // this is the item
		    LastLevel = theItem.GetTreeLevel // get the item's level
		    theLevelList.Append LastLevel  // get the item's level
		    theHIndexList.Append theItem.GetTreeHIndex // get the item's index on that level
		  next
		  // LastLevel should contain the  level of the bottom of the tree at this point
		  theShape = new StringShape
		  dim theWidth as integer = 500
		  //dim theWidth as integer = HSpace*(2^LastLevel) // this is the hypothetical maximum size of the bottom level
		  for i as integer = 0 to theLevelList.Ubound  // scan through the list
		    theShape.Text = theList(i).GetText
		    theShape.X = theWidth*theHIndexList(i)/(2^theLevelList(i) + 1) + HOffset
		    theShape.Y = theLevelList(i)*VSpace + VOffset
		    g.DrawObject(theShape)
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FillTop(ExprArray() as PhEd.Expression, theSubEx as PhEd.Expression)
		  // This method looks at the top operator in the operator expression stack and looks for
		  // empty slots, filling the appropriate slot with provided theSubEx expression and
		  // popping the top operator from the operator expression stack if it is full.
		  
		  dim theTop as PhEd.Expression // this will hold the top operator on the expression stack
		  if ExprArray.Ubound < 0 then // if the expression stack is empty
		    // then we have nothing to fill
		  else
		    theTop = ExprArray.Pop // pop the top expression on the expression stack
		    theTop.SetChild(theSubEx) // fill the appropriate child slot or raise the appropriate exception
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub findDragged(myList() as PhEd.Expression,X as integer, Y as integer)
		  // This method finds the term that the user is trying to drag.
		  // Iterate through the list of all terms and determine if the click was within their bounds 
		  // if yes, the user is trying to move that term.
		  
		  for i as integer= 0 to myList.Ubound
		    if Window1.binaryOpSelected then exit
		    if containsClick(X,Y, myList(i)) then
		      Window1.current = myList(i)
		      if X < Window1.equalSign_X then
		         Window1.left2right = true
		      else 
		        Window1.left2right = false
		      end if
		      if myList(i).getType = Expression.EType.BinaryOperator or myList(i).getType = Expression.EType.UnaryOperator then 
		        Window1.binaryOpSelected = true
		        Window1.draggedExpression= myList(i)
		      else 
		        Window1.draggedText = myList(i).GetText
		        Window1.draggedExpression= myList(i)
		      end if
		      myList(i).selected = true
		      Window1.isADrag = true
		    end if
		  next 
		  if Window1.isADrag = true then
		    showMenu(0)
		    Window1.Equation.selected = false
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub findSelected(myList() as PhEd.Expression, X as double, Y as double)
		  // This method determines which term the user is trying to select
		  // To do this, we check the relative position of the click with the X and Y of each term in the equation and
		  // of the equal sign
		  if Window1.Equation.myX < X and Window1.Equation.myX+ Window1.Equation.myWidth - 5 > X and Window1.Equation.myY > Y and  Window1.Equation.myY - Window1.Equation.myHeight  < Y  then 
		    functionBar(X,Y)
		  else
		    findTerm(myList,X,Y)
		  end if
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub findTerm(myList() as PhEd.Expression, X as double, Y as double)
		  // This method finds the term that was selected
		  // Iterate through all the terms and determine which term contains the coordinates of the click. 
		  // The character whose coordinates match is selected. 
		  for i as integer= 0 to myList.Ubound
		    if containsClick(X,Y,myList(i)) then
		      myList(i).selected = true
		      Window1.selectedExpr = myList(i)
		      showMenu(3)
		      Window1.Menu_X = X 
		      Window1.Menu_Y = Y + 10
		      Window1.Equation.selected = false 
		      Window1.isADrag = false
		    end if
		  next 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub functionBar(X as integer, Y as integer)
		  // handle case in which the function bar is selected
		  Window1.Menu_X = X 
		  Window1.Menu_Y = Y + 10
		  showMenu(2)
		  Window1.Equation.selected = true 
		  Window1.Expression = nil
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetFunctionScript(theName as Text) As XojoScript
		  // One can use this method both to see if the text provides a valid function name
		  // and get a XojoScript to execute a calculation. This method returns nil if the function
		  // is not found, and returns the XojoScript if it is. The point is that we can use this to
		  // provide a single well-defined way to list the valid unary scalar functions.
		  
		  dim xs as new XojoScript
		  Select case theName
		  Case "sqrt"
		    xs.Source = "theResult = sqrt(theArgVal)"
		  Case "abs"
		    xs.Source = "theResult = abs(theArgVal)"
		  Case "neg"
		    xs.Source = "theResult = -theArgVal"
		  Case "sin"
		    xs.Source = "theResult = sin(theArgVal)"
		  Case "cos"
		    xs.Source = "theResult = cos(theArgVal)"
		  Case "tan"
		    xs.Source = "theResult = tan(theArgVal)"
		  Case "exp"
		    xs.Source = "theResult = exp(theArgVal)"
		  Case "ln"
		    xs.Source = "theResult = log(theArgVal)"
		  Case "log"
		    xs.Source = "theResult = log(theArgVal)/log(10)"
		  Case "arcsin"
		    xs.Source = "theResult = asin(theArgVal)"
		  Case "arccos"
		    xs.Source = "theResult = acos(theArgVal)"
		  Case "arctan"
		    xs.Source = "theResult = atan(theArgVal)"
		  Else
		    xs = nil
		  End Select
		  return xs
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNextToken(CharAtIndex() as Text, ByRef indx as integer) As Text
		  // This method scans the expression for the next identifier starting at indx.
		  // It assumes that we are looking for an identifier, and so will accept + or - as a leading sign.
		  // It exits with either indx pointing at the offending operator character or beyond a terminating
		  // open parethesis, or beyond a leading sign.
		  // This method returns with the identifier (an empty identifier means that nothing was found).
		  
		  dim CharsInItem(-1) as text
		  dim ThisChar as text
		  dim OperatorList as text = "+-*/^()="  // These reserved characters can end an identifier
		  dim ReservedCode as integer
		  
		  // Loop as long as we don't have an operator character or a parenthesis or the end of the expression.
		  while indx <= CharAtIndex.Ubound
		    ThisChar = CharAtIndex(indx)  // Get the current character
		    ReservedCode = OperatorList.IndexOf(ThisChar)  // This checks to see if the current character is on the reserved list
		    if ReservedCode < 0 then // If the character is not on the list
		      CharsInItem.Append ThisChar // add the character to our current list of identifier characters
		      indx = indx + 1 // point to the next character
		    elseif ReservedCode = 0 or ReservedCode = 1 then // the character is a "+" or "-"
		      if CharsInItem.Ubound = -1 then // this is the first character, so is a leading sign
		        if ReservedCode = 1 then // if it is a negative sign
		          CharsInItem.Append ThisChar // append to the list (we simply ignore the positive sign altogether)
		          if indx < CharAtIndex.Ubound and not IsADigit(CharAtIndex(indx+1)) then // but if the next character is not a digit
		            // this is a unary minus operation
		            indx = indx + 1 // point to the next character
		            exit // we are done -- return the minus sign
		          end if
		        end if
		        indx = indx + 1 // otherwise point to the next character beyond the sign
		      elseif (CharsInItem.Ubound > 0) and IsADigit(CharsInItem(CharsInItem.Ubound-1)) and (CharsInItem(CharsInItem.Ubound) = "e") and (indx < CharAtIndex.Ubound) and IsADigit(CharAtIndex(indx + 1)) then
		        // If the sign is part of the pattern "<digit>e±<digit>" then we will interpret it as part of a floating point number.
		        CharsInItem.Append ThisChar // so add the sign to our current list of identifier characters
		        indx = indx + 1 // point to the next character
		      else // the + or - is not part of the identifier: it is a binary operator
		        exit // break out of the loop: we are done (indx points to the operator)
		      end if
		    elseif ReservedCode = 5 then // the item is an open parenthesis
		      CharsInItem.Append ThisChar // we append it to the identifier
		      indx = indx + 1 // point to the next character
		      exit // but we break out of the loop: this finishes the identifier
		    else  // if it is a breaking character that is not a + or - or ( then
		      exit // break out of the loop: we are done
		    end if // End check of possible breaking characters
		  wend
		  return Text.Join(CharsInItem,"")
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HandlePrecedence(theOp as text, OpStack() as Text, ECode() as Text)
		  // This method handles operator precedence.
		  // It assumes that theOp is a valid operator (it delivers
		  // an ExpressionException otherwise).
		  
		  dim PrecOfTop, PrecOfItem as integer
		  PrecOfItem = PrecedenceOf(theOp) // This is the precedence of the new item
		  do
		    if OpStack.Ubound < 0 then  // if the operator stack is empty,
		      OpStack.Append theOp // just push the new operator onto it
		      exit // and exit the loop
		    else
		      PrecOfTop = PrecedenceOf(OpStack(OpStack.Ubound)) // This is the precedence of stack top
		      if PrecOfTop = 99 then // this is a parenthesis or function
		        OpStack.Append theOp // just push the new operator onto it as if it were the beginning
		        exit // and we are done
		      elseif PrecOfItem > PrecOfTop then // if the new item has greater precendence than the stack top
		        OpStack.Append theOp // push the new operator onto the stack
		        exit // and we are done
		      elseif PrecOfItem = PrecOfTop then  // if their precedences are equal
		        ECode.Append OpStack.Pop// pop the last operator to the code
		        OpStack.Append theOp // and push the new one (this is consistent with left-to-right precedence)
		        exit // and we are done
		      else // the new operator has a lower precedence than the stack top
		        ECode.Append OpStack.Pop // pop the last operator to the code
		        // in this case, we loop again to compare theOp with the new top of stack (if we are not empty)
		      end if
		    end if
		  loop
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsADigit(digits as text) As Boolean
		  // determine if it is a natural number without any exponents or fractions
		  dim it as Xojo.Core.Iterator = digits.Characters.GetIterator
		  while it.MoveNext 
		    dim current as text = it.Value
		    if current.compare("0") < 0 or current.compare("9") > 0 then 
		      return false
		    end if
		  wend
		  return true
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsANumber(theItem as Text) As Boolean
		  // This method returns true if the entire item is an integer or a floating-point number
		  // It does it by doing a search using a Regular Expression.
		  dim re as new RegEx
		  dim rm as RegExMatch
		  re.SearchPattern = "^[+-]?([0-9]*\.?[0-9]+|[0-9]+\.?[0-9]*)([eE][+-]?[0-9]+)?$"
		  rm = re.Search(theItem)
		  return rm <> Nil
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function isAVariable(theItem as Text) As boolean
		  // determine if it is a variable
		  dim alphabet() as Text
		  alphabet = Array("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
		  return alphabet.indexOf(theItem) >=0 
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function isEquation(str as Text) As boolean
		  // determine if we have an equation
		  return str.indexOf("=") >=0
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsValidFunction(theFName as Text) As Boolean
		  // determine if it is a valid function
		  dim xs as XojoScript
		  xs = GetFunctionScript(theFName)
		  return not (xs = nil)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsValidOperator(theText as Text) As Boolean
		  // This returns true if the text is a valid binary operator token.
		  return (theText = "+") or (theText = "-") or (theText = "*") or (theText = "/") or (theText = "^") or (theText = "=")
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub moreFractions(expr as PhEd.Expression)
		  // fix fractions
		  dim myList() as PhEd.Expression = expr.GetList
		  for i as integer = 0 to myList.Ubound
		    if myList(i).getText = "/" and myList(i).GetLeftChild.GetText = "*" and myList(i).GetLeftChild.GetLeftChild.GetText = "/" then 
		      dim newLeft as PhEd.Expression = myList(i).GetLeftChild.GetLeftChild.copyNode
		      dim newRight as PhEd.Expression = createNode("/",myList(i).GetLeftChild.GetRightChild,myList(i).GetRightChild)
		      myList(i).setNode("*",newLeft,newRight)
		    end if
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function PrecedenceOf(theOp as Text) As Integer
		  // This method returns a precedence number for the supplied operator.
		  // Higher numbers have greater precedence.
		  
		  if theOp.Right(1) = "(" then
		    theOp = "(" // all functions are the same as parentheses
		  end if
		  Select Case theOp
		  Case "+", "-"
		    return 1
		  Case "/", "*"
		    return 2
		  Case "^"
		    return 3
		  Case "neg"
		    return 4
		  Case "("
		    return 99
		  Case "="  // included case where we have theOp to be =
		    return 0
		  Else
		    raise new ExpressionException("Operator has no defined precedence.")
		  End Select
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub showMenu(num as integer)
		  // Show the appropriate menu menu 
		  if num = 0 then 
		    Window1.functionMenu.Visible = false 
		    Window1.mainMenu.Visible = false 
		    Window1.PopupMenu1.Visible = false 
		    Window1.insertionMenu.Visible = false 
		    Window1.insertField.Visible = false 
		    Window1.insertButton.Visible = false
		  elseif num = 1 then 
		    Window1.functionMenu.Visible = true  
		    Window1.mainMenu.visible = false 
		    Window1.PopupMenu1.Visible = false 
		    Window1.insertionMenu.Visible = false 
		    Window1.insertField.Visible = false 
		    Window1.insertButton.Visible = false
		  elseif num = 2 then 
		    Window1.functionMenu.Visible = false  
		    Window1.mainMenu.visible = true 
		    Window1.PopupMenu1.Visible = false 
		    Window1.insertionMenu.Visible = false 
		    Window1.insertField.Visible = false 
		    Window1.insertButton.Visible = false
		  elseif num = 3 then 
		    Window1.functionMenu.Visible = false  
		    Window1.mainMenu.visible = false 
		    Window1.PopupMenu1.Visible = true
		    Window1.insertionMenu.Visible = false 
		    Window1.insertField.Visible = false 
		    Window1.insertButton.Visible = false
		  elseif num = 4 then 
		    Window1.insertionMenu.Visible = true 
		    Window1.functionMenu.Visible = false 
		    Window1.mainMenu.Visible = false 
		    Window1.PopupMenu1.Visible = false 
		    Window1.insertField.Visible = false 
		    Window1.insertButton.Visible = false
		  elseif num = 5 then 
		    Window1.insertionMenu.Visible = false 
		    Window1.functionMenu.Visible = false 
		    Window1.mainMenu.Visible = false 
		    Window1.PopupMenu1.Visible = false 
		    Window1.insertField.Visible = true 
		    Window1.insertButton.Visible = true 
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Text2Equation(theEquation as Text) As PhEd.Equation
		  // Turn text into an equation
		  // Parse the left and right side of the equation and then join them with the equal sign 
		  dim leftSide as Text = theEquation.Left(theEquation.indexOf("="))
		  dim rightSide as Text = theEquation.Right(theEquation.length - theEquation.indexOf("=") -1)
		  dim leftExpression as PhEd.Expression = Text2Expression(leftSide) 
		  dim rightExpression as PhEd.Expression = Text2Expression(rightSide) 
		  dim equation as Equation = new Equation()
		  equation.SetExpressions(leftExpression,rightExpression)
		  if not equation.isLegal then Window1.ExceptionType = 5
		  return equation
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Text2Expression(theExpr as Text) As PhEd.Expression
		  // This method converts a calculator-style text expression to a PhEd.Expression.
		  // The basic algorithm is as follows. First we convert theExpr to a postfix expression array.
		  // We then define an expression stack that will store operator expressions that have open slots that need filling.
		  // Then we iterate the following :
		  // (1) Pop the top (last) item from the postix array and create the corresponding type of expression
		  // (2) If it is a variable, constant, or literal, then it needs to go into an unfilled slot in a previously defined expression
		  //      (unless it is the ONLY expression
		  //      The top of the expression stack contains the last defined expression having unfilled slots
		  //      (a) If that top expression is a binary operator then we first fill its right slot, or if that one is full, its left slot
		  //           with the newly-created expression.
		  //      (b) If that top expression is a unary operator, then we fill its only slot with the newly created expression.
		  //      (c) If that top expression is now full, we pop it from the expression stack (which only contains unfilled operators).
		  // (3) If the new expression is a binary or unary operator itself, then we do step (2) but also
		  //      (a) we push the new operator onto the operator stack, because its slots are not yet full.
		  //      (b) we check whether it is the first operator -- the first operator is the root of the whole tree.
		  // (4) When we have no postfix items left, we return the root expression.
		  // At present, this only works for scalar expressions
		  dim Postfix(-1) as Text  // define postfix array
		  dim ExprArray(-1) as PhEd.Expression // this is the expression stack
		  dim theResult as PhEd.Expression // this will be the final result
		  dim theSubEx as PhEd.Expression // this will hold a new subexpression
		  dim theItem as text // this will be the current item from the postfix array
		  dim FirstItem as Boolean = true // this is a flag indicating the first item
		  Postfix = Text2Postfix(theExpr) // convert expression to a postfix array
		  if Postfix = nil or Postfix.Ubound < 0 then 
		     return new PhEd.Expression()
		  end if
		  do
		    theItem = Postfix.Pop // get the top (last) item in the postfix array
		    if IsValidOperator(theItem) then
		      theSubEx = new PhEd.Expression(theItem) // create a new binary operator expression
		      if FirstItem then theResult = theSubEx // if this is the first operator, then it is the root operator
		      FillTop(ExprArray, theSubEx) //put the newly created item into the top expression on the expression stack
		      ExprArray.Append theSubEx // push whatever the new expression we have created onto the expression stack
		      ExprArray.Append theSubEx // twice, because we have two slots to fill
		    elseif IsValidFunction(theItem) then // if the item is a function or negation operator
		      theSubEx = new PhEd.Expression(theItem) // create a new unary operator expression
		      if FirstItem then theResult = theSubEx // if this is the first operator then it is the root operator
		      FillTop(ExprArray, theSubEx) //put the newly created item into the top expression on the expression stack
		      ExprArray.Append theSubEx // push whatever new operation we have created onto the expression stack
		    elseif IsANumber(theItem) then// if the item is a number
		      theSubEx = new PhEd.Expression(theItem) // create a new literal expression
		      if FirstItem then theResult = theSubEx  // if this is the first operator, then it is the root operator
		      FillTop(ExprArray, theSubEx) // and put it into an appropriate empty subexpression in the top operator in the stack
		      // But we don't push anything to the expression stack
		    else // we must have a variable
		      theSubEx = new PhEd.Expression(theItem)  // create a new variable item
		      if FirstItem then theResult = theSubEx  // if this is the first operator, then it is the root operator
		      FillTop(ExprArray, theSubEx) // and put it into an appropriate empty subexpression in the top operator in the stack
		      // But we don't push anything to the expression stack
		    end if
		    FirstItem = false // we no longer are looking at the first item
		  loop until Postfix.Ubound < 0 // repeat until the postfix stack is empty
		  // if theResult is nil, then the the top (final) item in the postfix was not a legal kind of operator
		  if theResult = nil then
		    return new PhEd.Expression()
		  end if
		  // we should complete the expression with nothing left in the expression array.
		  if ExprArray.Ubound >=0 then  
		    return new PhEd.Expression()
		  end if
		  if not theResult.isLegal then Window1.ExceptionType = 4
		  dim myList() as PhEd.Expression = theResult.getList
		  return theResult // if we make it to here, we should have a valid result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Text2Postfix(theText as Text) As Text()
		  // This method takes text in calculator-style format and
		  // returns an array of text tokens in postfix format.
		  // The basic algorithm goes as follows (it uses a stack that holds operators):
		  // (1) Put any operand in the output array in the order that it arrives.
		  // (2) With an operator, so the following
		  //      (a) If the operator stack is empty, or the the top operator on the stack is "(" or a function
		  //           then push the new operator onto the operator stack
		  //      (b) If the new operator is "(" or a function, push it onto the stack
		  //      (c) If the new operator is ")", pop the stack to the output array until you get to "(". Discard the parentheses.
		  //      (d) If the new operator has higher precedence than that on the stack's top, push it onto the stack.
		  //      (e) If the new operator has equal precenced to that on the stack's top, pop the top of the stack
		  //           to the output array and push the new operator. This honors left-to-right prececence.
		  //      (f)  If the new operator has lower precedence than that on the stack's top, pop the top of the stack
		  //            to the output array. Then test the new operator against the the new top of stack and iterate (d)-(f).
		  // (3) When you reach the end, pop and output all remaining operators. There should be no parentheses.
		  
		  theText = theText.Trim  // Trim Unicode whitespace from expression
		  theText = theText.ReplaceAll(" ","")  // trim any interior spaces
		  
		  dim ExpressionCode(-1) as Text // this will be the list of items
		  dim OpStack(-1) as Text // this will be the operator stack
		  dim indx as integer // This points to the character being analyzed
		  dim ExpectingOperand as Boolean = true  // the first item in the expression should be an operand
		  dim theItem as Text // this will hold tokens (numbers, variables, or operators)
		  theText = theText.Trim  // Trim Unicode whitespace from expression
		  theText = theText.ReplaceAll(" ","")  // trim any interior spaces
		  dim CharAtIndex(-1) as text // this will hold the expression as an array of characters
		  CharAtIndex = theText.Split // Split the expression into characters
		  // This is the main loop that looks at the expression character by character
		  while indx <= UBound(CharAtIndex) // while we are still within the expression
		    if ExpectingOperand then // if we are expecting an operand
		      theItem = GetNextToken(CharAtIndex, indx) // Get the next token
		      // after this indx points to the next item to get: either the operator that ended the token
		      // or *after* an opening parenthesis or a negative
		      if theItem.Empty then // if we have no item at all, this is a syntax problem
		        Window1.ExceptionType = 4
		        dim wrongResult(-1) as Text
		        return wrongResult
		        //raise new ExpressionException("Expected an identifier") // so raise an exception
		      elseif theItem = "(" then
		        OpStack.Append "("  // put this on the operator stack
		        // Note that we are still looking for an operand
		      elseif theItem.Right(1) = "(" then // perhaps this is a function
		        if IsValidFunction(theItem.Left(theItem.Length-1)) then // if it is a valid function, then
		          OpStack.Append theItem.Left(theItem.Length) // put its identifier on the ExpressionCode list, including the "("
		          // Note that we are still looking for an operand
		        else
		          Window1.ExceptionType = 4
		          dim wrongResult(-1) as Text
		          return wrongResult
		          //raise new ExpressionException("Invalid function")
		        end if
		      elseif IsANumber(theItem) then
		        ExpressionCode.Append theItem // put it on the ExpressionCode list
		        ExpectingOperand = false // and we are now looking for an operator
		      elseif theItem = "-" then // it is a leading minus, which effectively is an operator
		        HandlePrecedence("neg", OpStack, ExpressionCode)
		        // and we are still looking for an operand
		      else // it must be an identifier of some kind
		        // might do some checking of valid variable names here
		        if isAVariable(theItem)  then 
		          ExpressionCode.Append theItem // put it on the ExpressionCode list
		          ExpectingOperand = false // and we are now looking for an operator
		        else 
		          Window1.ExceptionType = 1
		          dim wrongResult(-1) as Text
		          return wrongResult
		        end if
		      end if
		    else
		      theItem = CharAtIndex(indx)  // Get the operator
		      if IsValidOperator(theItem) then // if the operator is valid
		        HandlePrecedence(theItem, OpStack, ExpressionCode) // Deal with it according to precedence
		        ExpectingOperand = true // after an operator, we expect an operand
		      elseif theItem = ")" then // if it is a right parenthesis, we pop operators back to the last "("
		        do
		          if OpStack.Ubound < 0 then // if the stack is empty, this is an error
		            Window1.ExceptionType = 0
		            dim wrongResult(-1) as Text
		            return wrongResult
		            //raise new ExpressionException("Unbalanced parentheses")
		          else
		            dim TopOp as text = OpStack.Pop // pop top operator
		            if TopOp = "(" then  // we have found a left parenthesis
		              exit  // so we are done popping
		            elseif TopOp.Right(1) = "(" then // if we have found the end of function
		              ExpressionCode.Append TopOp.Left(TopOp.Length-1)  // output the function name
		              exit // and we are done popping
		            else
		              ExpressionCode.Append TopOp  // pop any other operator
		              // and check the next
		            end if
		          end if
		        loop
		        // now we are still expecting an operator
		      else // Getting here is a syntax error: there are no other operators
		        Window1.ExceptionType = 4
		        dim wrongResult(-1) as Text
		        return wrongResult
		        //raise new ExpressionException("Expected an operator")
		      end if // end check for valid operator
		      indx = indx + 1  // point to the next character and go on
		    end if // end expecting operator
		  wend
		  
		  //At the end of the expression, we should be looking for an operand
		  if ExpectingOperand then
		    Window1.ExceptionType = 4
		    dim wrongResult(-1) as Text
		    return wrongResult
		    //raise new ExpressionException("Unexpected end of expression")
		  end if
		  
		  // Now pop all the remaining operators. (We should have no parentheses left.)
		  while OpStack.Ubound >= 0
		    dim StackItem as Text = OpStack.Pop
		    if StackItem = ")" or StackItem = "(" then
		      Window1.ExceptionType = 0
		      dim wrongResult(-1) as Text
		      return wrongResult
		      //raise new ExpressionException("Unbalanced parentheses")
		    else
		      ExpressionCode.Append StackItem
		    end if
		  wend
		  return ExpressionCode
		End Function
	#tag EndMethod


	#tag Constant, Name = kSuperscriptChar, Type = Text, Dynamic = False, Default = \"@", Scope = Public
	#tag EndConstant


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
	#tag EndViewBehavior
End Module
#tag EndModule
