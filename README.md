
To generate the lexer: 

fslex PALexer.fsl --unicode
 

To generate the parser:

fsyacc PAParser.fsp --module PAParser


To run the program write in a terminal in the folder PracticalAssignment 

dotnet fsi PA.fsx 

Key files are all in the PracticalAssignment folder.


For task1 the hope is that every input should be handled by an output which is the same as the input, though ofcause only within the limits of the GCL commands. 