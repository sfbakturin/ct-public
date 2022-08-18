#pragma once

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define GET(OBJ) (i < OBJ.capacity ? LN::to_int(OBJ[i]) : 0)
#define GETJ(OBJ) (j < OBJ.capacity ? LN::to_int(OBJ[j]) : 0)
#define EXIST(OBJ) (function.find(OBJ) != function.end())
#define BINARY                   \
	LN right = expression.top(); \
	expression.pop();            \
	LN left = expression.top();  \
	expression.pop()

#define UNARY                    \
	LN unary = expression.top(); \
	expression.pop()

#define BOOLEAN(FLAG) (FLAG ? expression.emplace("1") : expression.emplace("0"))