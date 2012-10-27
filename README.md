CSC151.02 2010S Project Proposal
Group: David Cowden, Sammy Huang, Alex Hsieh, Reed Caron
3 May 2010


Design Statement
We will be exploring the union between organic shapes and set mathematical formulas.  These formulas could define fractal patterns that generate flowing, distinct forms while at the same time allowing us to automate and parameterize the image creation.  Of such fractal formulas, we have chosen to make an image-creating procedure based on the Julia fractal set because of its elegance.  However, to make our artwork an even more visually stimulating exploration of forms and color harmony, we will add two more elements to our procedure along with the Julia fractal component.  The second element will be to create grids of circles and squares to explore the dynamics between depth, texture, grid-implying rigidity, and the free-flowing curves of the Julia fractal.  The third element will be to invert lighten the color of specific pixels that correspond to the shape of a Dragon Curve fractal.  These three elements will be combined together to create a unique blend of shapes and colors that paradoxically expresses both harmony and conflict.


Technique Statement
Our image-creating procedure will then be divided into three parts.  The procedure will run these parts in their respective order to create the final image.
1.Julia Set (procedure: julia)
1.Our basic approach will be to use image-compute-pixels as the primary action along with a recursive definition that adjusts according to the Julia formula.
1.The Julia Set is based on the formula Jn+1 = Jn2   + k, where k = n/1000.
2.Set and thus shape of image is altered according to n.
3.Colors are based on the number of recursions it takes each pixel to escape the set.
2.Circles and Squares Grid (procedures: circles, mesh-grid)
1.For each ones digit (0-9) of n, we will create a procedure that makes a different amount of small circles and squares in a grid.  
1.The size of each circle or square in a grid is set. 
2.The procedures will recurse through an area of the image defined by the parameters, making selections based on the area's row/column and moving forward by offsets until it reaches the end width of the area.
3.Circles will be created with image-select-ellipse! and then selection-transform! with rgb-lighter.  
4.Squares will be created with image-select-rectangle! and then image-select-inverse! to select the outline of the squares.  We will then use selection-transform! with rgb-darker to give the impression of squares. 
3.Dragon Curve (procedure: dragon)
1.Will be created using turtle-esque graphics and recursive definition.  
1.We will define our own turtle system affectionately named 'pandas'
2.Each panda will be able to take a color-transforming procedure (e.g. invert) and apply it to the color of the pixel it travels over effectively changing the color of that pixel
3.The pandas will travel according to the shape of a Dragon Curve.  The number of the trans-iterations will be altered by n. 
4.We will also make another Dragon Curve-esque fractal by the procedure name dragoons.  This procedure will slightly alter a rewrite rule in the procedure dragon to add variance in our design. 

	



Reflection
	We know our procedure creates 1,000 distinct images because for every n in that range, there is a unique Julia set correspondence, meaning that there is a distinct image given enough pixels.  Our original proposal called for creating a Sierpinski carpet of circles for our second element, however, we did not follow through with this idea because it created an image that was too symmetric to comply with our design vision.  Instead, we altered our approach to the second element to create orderly grids of circles and squares which would be subtle in the image and not distract from the organic forms of the other shapes.  For each ones digit of n (0-9), there is a different amount of circles (while the amount of squares from mesh-grid will always be a grid of 4px by 4px squares for aesthetic stability) created using the procedure circles.  Allowing the circle grids to correspond to 1,000 different values would be too tedious without a randomization component, therefore our procedure simplifies its degree of variance by producing the same amount of circles for an example such as n = 1, 11, 21... 991.  Our Dragon Curve procedure will create a different image for each tens digit of n because n determines the amount of recursions/iterations of the fractal.  It could technically create an unlimited number of curves, but anything above 17 would take more processing power/time than we have available.  Therefore, we simply choose to use the tens digit of n.  
