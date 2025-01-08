using System.Text.RegularExpressions;

namespace GMLSharpener;

internal enum TokenType
{
    None,
    Eof,
    Unknown,
    Identifier,
    Real,
    StringLiteral,
    Boolean,
    Break,
    Continue,
    If,
    Then,
    Else,
    While,
    Do,
    Until,
    For,
    BitwiseComplement,
    Not,
    Inequality,
    Mod,
    BitwiseXor,
    LogicalXor,
    XorAssignment,
    BitwiseAnd,
    LogicalAnd,
    AndAssignment,
    Multiply,
    MultiplyAssignment,
    OpeningParenthesis,
    ClosingParenthesis,
    Minus,
    SubtractionAssignment,
    Plus,
    AdditionAssignment,
    Assignment,
    Equality,
    OpeningCurlyBrace,
    ClosingCurlyBrace,
    OpeningSquareBracket,
    ClosingSquareBracket,
    BitwiseOr,
    LogicalOr,
    OrAssignment,
    Colon,
    Semicolon,
    LessThan,
    LessThanOrEqual,
    ShiftLeft,
    ShiftRight,
    GreaterThan,
    GreaterThanOrEqual,
    Comma,
    Dot,
    Divide,
    DivideAssignment,
    Div,
    Var,
    Globalvar,
    Repeat,
    Switch,
    Case,
    Default,
    Exit,
    With,
    Return,
    Increment,
    Decrement,
    Constructor,
    New,
    Function,
    Enum
}

internal class TokenRecord
{
    public TokenType Type; 
    public string Value = "";
    
    public TokenRecord(TokenType type)
    {
        Type = type;
    }
    public TokenRecord(TokenType type, string value)
    {
        Type = type;
        Value = value;
    }

    public Token New(int line, int column)
    {
        return new Token(Type, Value, line, column);
    }
}

internal class Token
{
    public TokenType Type; 
    public int Line; 
    public int Column; 
    public string Value = "";
    
    public Token(TokenType type, int line, int column) { 
        Type = type; 
        Line = line; 
        Column = column; 
    }
    
    public Token(TokenType type, string lexeme, int line, int column) { 
        Type = type; 
        Line = line; 
        Column = column; 
        Value = lexeme; 
    }

    public override string ToString()
    {
        return Value;
    }
}

#region Value Tokens
internal class RealToken : Token
{
    public new float Value;
    
    public RealToken(float val, int line, int col) : base(TokenType.Real, line, col)
    {
        base.Value = val.ToString();
        Value = val;
    }
}

internal class StringToken : Token
{
    public new string Value;
    
    public StringToken(string val, int line, int col) : base(TokenType.StringLiteral, line, col)
    {
        base.Value = val;
        Value = val;
    }
}

internal class BooleanToken : Token
{
    public new bool Value;
    
    public BooleanToken(bool val, int line, int col) : base(TokenType.Boolean, line, col)
    {
        base.Value = val ? "true" : "false";
        Value = val;
    }
}
#endregion

/// <summary>
/// 
/// </summary>
internal partial class Lexer;

internal partial class Lexer
{
    private const int EOF = -1;
    public static Dictionary<string, TokenRecord> Reserved;
    public static Dictionary<string, TokenRecord> Tokens;
    
    private static class TokenTrie
    {
        private static readonly List<Dictionary<int, int>> tree = [[]];
        private static readonly List<bool> end = [false];

        public static void Insert(string word)
        {
            int p = 0;
            char[] chars = word.ToCharArray();
            for (int i = 0; i < chars.Length; i++) {
                int c = chars[i];
                if (!tree[p].TryGetValue(c, out _))
                {
                    tree[p][c] = tree.Count;
                    tree.Add([]);
                    end.Add(false);
                }
                p = tree[p][c];
            }
            end[p] = true;
        }

        public static bool TryMatch(Lexer lexer, out string? token)
        {
            int p = 0;
            int c = lexer.PeekChar;
            string t = "";
            while (tree[p].TryGetValue(c, out int ind))
            {
                p = ind;
                t += (char)lexer.ReadChar;
                c = lexer.PeekChar;
            }
            token = t;
            return end[p];
        }
    }

    private static class Keywords
    {
        public static readonly TokenRecord Break                   = new(TokenType.Break,                 "break"      );
        public static readonly TokenRecord Continue                = new(TokenType.Continue,              "continue"   );
        public static readonly TokenRecord Return                  = new(TokenType.Return,                "return"     );
        public static readonly TokenRecord If                      = new(TokenType.If,                    "if"         );
        public static readonly TokenRecord Then                    = new(TokenType.Then,                  "then"       );
        public static readonly TokenRecord Else                    = new(TokenType.Else,                  "else"       );
        public static readonly TokenRecord While                   = new(TokenType.While,                 "while"      );
        public static readonly TokenRecord Until                   = new(TokenType.Until,                 "until"      );
        public static readonly TokenRecord Do                      = new(TokenType.Do,                    "do"         );
        public static readonly TokenRecord For                     = new(TokenType.For,                   "for"        );
        public static readonly TokenRecord NotWord                 = new(TokenType.Not,                   "not"        );
        public static readonly TokenRecord And                     = new(TokenType.LogicalAnd,            "and"        );
        public static readonly TokenRecord Or                      = new(TokenType.LogicalOr,             "or"         );
        public static readonly TokenRecord Xor                     = new(TokenType.LogicalXor,            "xor"        );
        public static readonly TokenRecord Repeat                  = new(TokenType.Repeat,                "repeat"     );
        public static readonly TokenRecord With                    = new(TokenType.With,                  "with"       );
        public static readonly TokenRecord Div                     = new(TokenType.Div,                   "div"        );
        public static readonly TokenRecord Mod                     = new(TokenType.Mod,                   "mod"        );
        public static readonly TokenRecord Var                     = new(TokenType.Var,                   "var"        );
        public static readonly TokenRecord Globalvar               = new(TokenType.Globalvar,             "globalvar"  );
        public static readonly TokenRecord Switch                  = new(TokenType.Switch,                "switch"     );
        public static readonly TokenRecord Case                    = new(TokenType.Case,                  "case"       );
        public static readonly TokenRecord Default                 = new(TokenType.Default,               "default"    );
        public static readonly TokenRecord Exit                    = new(TokenType.Exit,                  "exit"       );
        public static readonly TokenRecord Begin                   = new(TokenType.OpeningCurlyBrace,     "begin"      );
        public static readonly TokenRecord End                     = new(TokenType.ClosingCurlyBrace,     "end"        );
        public static readonly TokenRecord Self                    = new(TokenType.Identifier,            "self"       );
        public static readonly TokenRecord Other                   = new(TokenType.Identifier,            "other"      );
        public static readonly TokenRecord All                     = new(TokenType.Identifier,            "all"        );
        public static readonly TokenRecord Noone                   = new(TokenType.Identifier,            "noone"      );
        public static readonly TokenRecord Global                  = new(TokenType.Identifier,            "global"     );
        public static readonly TokenRecord BitwiseComplement       = new(TokenType.BitwiseComplement,     "~"          );
        public static readonly TokenRecord Not                     = new(TokenType.Not,                   "!"          );
        public static readonly TokenRecord Inequality              = new(TokenType.Inequality,            "!="         );
        public static readonly TokenRecord BitwiseXor              = new(TokenType.BitwiseXor,            "^"          );
        public static readonly TokenRecord LogicalXor              = new(TokenType.LogicalXor,            "^^"         );
        public static readonly TokenRecord XorAssignment           = new(TokenType.XorAssignment,         "^="         );
        public static readonly TokenRecord BitwiseAnd              = new(TokenType.BitwiseAnd,            "&"          );
        public static readonly TokenRecord LogicalAnd              = new(TokenType.LogicalAnd,            "&&"         );
        public static readonly TokenRecord AndAssignment           = new(TokenType.AndAssignment,         "&="         );
        public static readonly TokenRecord Multiply                = new(TokenType.Multiply,              "*"          );
        public static readonly TokenRecord MultiplyAssignment      = new(TokenType.MultiplyAssignment,    "*="         );
        public static readonly TokenRecord OpeningParenthesis      = new(TokenType.OpeningParenthesis,    "("          );
        public static readonly TokenRecord ClosingParenthesis      = new(TokenType.ClosingParenthesis,    ")"          );
        public static readonly TokenRecord Minus                   = new(TokenType.Minus,                 "-"          );
        public static readonly TokenRecord SubtractionAssignment   = new(TokenType.SubtractionAssignment, "-="         );
        public static readonly TokenRecord Plus                    = new(TokenType.Plus,                  "+"          );
        public static readonly TokenRecord AdditionAssignment      = new(TokenType.AdditionAssignment,    "+="         );
        public static readonly TokenRecord Assignment              = new(TokenType.Assignment,            "="          );
        public static readonly TokenRecord Equality                = new(TokenType.Equality,              "=="         );
        public static readonly TokenRecord OpeningCurlyBrace       = new(TokenType.OpeningCurlyBrace,     "{"          );
        public static readonly TokenRecord ClosingCurlyBrace       = new(TokenType.ClosingCurlyBrace,     "}"          );
        public static readonly TokenRecord OpeningSquareBracket    = new(TokenType.OpeningSquareBracket,  "["          );
        public static readonly TokenRecord ClosingSquareBracket    = new(TokenType.ClosingSquareBracket,  "]"          );
        public static readonly TokenRecord BitwiseOr               = new(TokenType.BitwiseOr,             "|"          );
        public static readonly TokenRecord LogicalOr               = new(TokenType.LogicalOr,             "||"         );
        public static readonly TokenRecord OrAssignment            = new(TokenType.OrAssignment,          "|="         );
        public static readonly TokenRecord Colon                   = new(TokenType.Colon,                 ":"          );
        public static readonly TokenRecord Semicolon               = new(TokenType.Semicolon,             ";"          );
        public static readonly TokenRecord LessThan                = new(TokenType.LessThan,              "<"          );
        public static readonly TokenRecord LessThanOrEqual         = new(TokenType.LessThanOrEqual,       "<="         );
        public static readonly TokenRecord ShiftLeft               = new(TokenType.ShiftLeft,             "<<"         );
        public static readonly TokenRecord GreaterThan             = new(TokenType.GreaterThan,           ">"          );
        public static readonly TokenRecord GreaterThanOrEqual      = new(TokenType.GreaterThanOrEqual,    ">="         );
        public static readonly TokenRecord ShiftRight              = new(TokenType.ShiftRight,            ">>"         );
        public static readonly TokenRecord Comma                   = new(TokenType.Comma,                 ","          );
        public static readonly TokenRecord Dot                     = new(TokenType.Dot,                   "."          );
        public static readonly TokenRecord Divide                  = new(TokenType.Divide,                "/"          );
        public static readonly TokenRecord DivideAssignment        = new(TokenType.DivideAssignment,      "/="         );
        public static readonly TokenRecord Increment               = new(TokenType.Increment,             "++"         );
        public static readonly TokenRecord Decrement               = new(TokenType.Decrement,             "--"         );
        public static readonly TokenRecord Constructor = new(TokenType.Constructor,           "constructor");
        public static readonly TokenRecord New         = new(TokenType.New,                   "new"        );
        public static readonly TokenRecord Function    = new(TokenType.Function,              "function"   );
        public static readonly TokenRecord Enum        = new(TokenType.Enum,                  "enum"       );
    }

    private int Line    = 1;
    private int Column  = 1;

    private readonly StringReader Reader;
    private int PeekChar { get => Reader.Peek(); }
    private int ReadChar
    {
        get
        {
            var chr = Reader.Read();
            if (chr == '\n')
            {
                Line ++;
                Column = 1;
            }
            else Column ++;
            return chr;
        }
    }

    public Lexer(string code)
    {
        Reader = new(code);
    }

    static Lexer () {
        System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(typeof(Keywords).TypeHandle);
        Reserved = [];
        foreach (var field in typeof(Keywords).GetFields())
        {
            var tokenRecord = (TokenRecord)field.GetValue(null)!;
            Reserved[tokenRecord.Value] = tokenRecord;
        }
        Tokens = [];
        #region Tokens
        Tokens.Add("~", new(TokenType.BitwiseComplement));
        Tokens.Add("(", new(TokenType.OpeningParenthesis));
        Tokens.Add(")", new(TokenType.ClosingParenthesis));
        Tokens.Add("{", new(TokenType.OpeningCurlyBrace));
        Tokens.Add("}", new(TokenType.ClosingCurlyBrace));
        Tokens.Add("[", new(TokenType.OpeningSquareBracket));
        Tokens.Add("]", new(TokenType.ClosingSquareBracket));
        Tokens.Add(";", new(TokenType.Semicolon));
        Tokens.Add(",", new(TokenType.Comma));

        Tokens.Add(":", new(TokenType.Colon));
        Tokens.Add(":=", new(TokenType.Assignment));

        Tokens.Add("=", new(TokenType.Assignment));
        Tokens.Add("==", new(TokenType.Equality));

        Tokens.Add("!", new(TokenType.Not));
        Tokens.Add("!=", new(TokenType.Inequality));

        Tokens.Add("*", new(TokenType.Multiply));
        Tokens.Add("*=", new(TokenType.MultiplyAssignment));

        Tokens.Add("+", new(TokenType.Plus));
        Tokens.Add("+=", new(TokenType.AdditionAssignment));

        Tokens.Add("-", new(TokenType.Minus));
        Tokens.Add("-=", new(TokenType.SubtractionAssignment));

        Tokens.Add("/", new(TokenType.Divide));
        Tokens.Add("/=", new(TokenType.DivideAssignment));

        Tokens.Add("++", new(TokenType.Increment));
        Tokens.Add("--", new(TokenType.Decrement));

        Tokens.Add("&", new(TokenType.BitwiseAnd));
        Tokens.Add("&&", new(TokenType.LogicalAnd));
        Tokens.Add("&=", new(TokenType.AndAssignment));

        Tokens.Add("|", new(TokenType.BitwiseOr));
        Tokens.Add("||", new(TokenType.LogicalOr));
        Tokens.Add("|=", new(TokenType.AndAssignment));

        Tokens.Add("^", new(TokenType.BitwiseXor));
        Tokens.Add("^^", new(TokenType.LogicalXor));
        Tokens.Add("^=", new(TokenType.XorAssignment));
        
        Tokens.Add("<", new(TokenType.LessThan));
        Tokens.Add("<=", new(TokenType.LessThanOrEqual));
        Tokens.Add("<<", new(TokenType.ShiftLeft));

        Tokens.Add(">", new(TokenType.GreaterThan));
        Tokens.Add(">=", new(TokenType.GreaterThanOrEqual));
        Tokens.Add(">>", new(TokenType.ShiftRight));

        Tokens.Add("<>", new(TokenType.Inequality));
        #endregion
        foreach (var key in Tokens.Keys)
        {
            TokenTrie.Insert(key);
        }
    }
}

internal partial class Lexer
{
    public Token? Scan()
    {
        // Remove all white spaces like trim
        while (PeekChar != EOF && char.IsWhiteSpace((char)PeekChar))
            _ = ReadChar;
        if (PeekChar == EOF) return new Token(TokenType.Eof, Line, Column);

        if (PeekChar == '\"')
        {
            _ = ReadChar;
            string literal = "";
            while (PeekChar != EOF && PeekChar != '\"')
            {
                literal += (char)ReadChar;
            }
            if (PeekChar == EOF) throw new Exception();
            _ = ReadChar;
            return new StringToken(literal, Line, Column);
        }

        if (PeekChar == '.' || char.IsNumber((char)PeekChar))
        {
            string literal = "";
            if (PeekChar == '.')
            {
                _ = ReadChar;
                if (!char.IsNumber((char)PeekChar))
                {
                    return new Token(TokenType.Dot, Line, Column);
                }
                literal += '.';
            }
            while (PeekChar != EOF && (PeekChar == '.' || char.IsNumber((char)PeekChar)))
            {
                literal += (char)ReadChar;
            }
            return new RealToken(float.Parse(literal), Line, Column);
        }

        if (PeekChar == '$')
        {
            _ = ReadChar;
            string literal = "0x";
            while (PeekChar != EOF && char.IsAsciiHexDigit((char)PeekChar))
            {
                literal += ReadChar;
            }
            return new RealToken(float.Parse(literal, System.Globalization.NumberStyles.HexNumber), Line, Column);
        }
        
        if (PeekChar == '/')
        {
            _ = ReadChar;
            switch (PeekChar)
            {
                case '=':
                _ = ReadChar;
                return new Token(TokenType.DivideAssignment, Line, Column);
                
                case '/':
                _ = ReadChar;
                Reader.ReadLine();
                Line ++; Column = 1;
                return null;

                case '*':
                _ = ReadChar;
                while (PeekChar != EOF)
                {
                    if (ReadChar == '*' && PeekChar == '/')
                    {
                        _ = ReadChar;
                        return null;
                    }
                }
                throw new Exception();

                default:
                return new Token(TokenType.Divide, Line, Column);
            }
        }
        
        if (PeekChar == '_' || char.IsLetter((char)PeekChar))
        {
            string literal = "";
            while (PeekChar != EOF && (PeekChar == '_' || char.IsLetterOrDigit((char)PeekChar)))
            {
                literal += (char)ReadChar;
            }
            if (Reserved.TryGetValue(literal, out var reserved)) return reserved.New(Line, Column);
            return literal switch
            {
                "true"  => new BooleanToken(true , Line, Column),
                "false" => new BooleanToken(false, Line, Column),
                _ => new Token(TokenType.Identifier, literal, Line, Column)
            };
        }

        if (TokenTrie.TryMatch(this, out string? token))
        {
            return Tokens[token!].New(Line, Column);
        }

        throw new Exception();
    }
}
