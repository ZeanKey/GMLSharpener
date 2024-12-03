using System.Text.RegularExpressions;

namespace GMLSharpener;

internal enum TokenKind
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

internal class Token
{
    public TokenKind Type; 
    public int Line; 
    public int Column; 
    public string? Lexeme = "";
    
    public Token(TokenKind kind, int line, int column) { 
        Type = kind; 
        Line = line; 
        Column = column; 
    }
    public Token(TokenKind kind, string lexeme, int line, int column) { 
        Type = kind; 
        Line = line; 
        Column = column; 
        Lexeme = lexeme; 
    }

    public override string ToString()
    {
        return Lexeme ?? "";
    }
}

#region Value Tokens
internal class RealToken : Token
{
    public float? Value;
    
    public RealToken(float? val, string? lexeme, int line, int col) : base(TokenKind.Real, line, col)
    {
        Lexeme = lexeme;
        Value = val;
    }

    public override string ToString()
    {
        if ((this.Lexeme != null) && (this.Lexeme != ""))
            return this.Lexeme;
        return Value.ToString() ?? "";
    }
}

internal class StringLiteralToken : Token
{
    public string? Value;
    
    public StringLiteralToken(string? val, string lexeme, int line, int col) : base(TokenKind.StringLiteral, line, col)
    {
        Lexeme = lexeme;
        Value = val;
    }

    public override string ToString()
    {
        if ((Lexeme != null) && (Lexeme != ""))
            return Lexeme;
        return Value!.ToString();
    }
}

internal class BooleanToken : Token
{
    public bool Value;
    
    public BooleanToken(bool val, int line, int col) : base(TokenKind.Boolean, line, col)
    {
        Lexeme = val ? "true" : "false";
        Value = val;
    }

    public override string ToString()
    {
        return Lexeme!;
    }
}
#endregion

internal static class Tokens
{
    public static readonly Token Unknown = new Token(TokenKind.Unknown, 0, 0);
    public static readonly Token Eof = new Token(TokenKind.Eof, 0, 0);
    public static readonly Token None = new Token(TokenKind.None, 0, 0);
    public static readonly Token Break                   = new Token ( TokenKind.Break,                 "break"      , 0, 0);
    public static readonly Token Continue                = new Token ( TokenKind.Continue,              "continue"   , 0, 0);
    public static readonly Token Return                  = new Token ( TokenKind.Return,                "return"     , 0, 0);
    public static readonly Token If                      = new Token ( TokenKind.If,                    "if"         , 0, 0);
    public static readonly Token Then                    = new Token ( TokenKind.Then,                  "then"       , 0, 0);
    public static readonly Token Else                    = new Token ( TokenKind.Else,                  "else"       , 0, 0);
    public static readonly Token While                   = new Token ( TokenKind.While,                 "while"      , 0, 0);
    public static readonly Token Until                   = new Token ( TokenKind.Until,                 "until"      , 0, 0);
    public static readonly Token Do                      = new Token ( TokenKind.Do,                    "do"         , 0, 0);
    public static readonly Token For                     = new Token ( TokenKind.For,                   "for"        , 0, 0);
    public static readonly Token NotWord                 = new Token ( TokenKind.Not,                   "not"        , 0, 0);
    public static readonly Token And                     = new Token ( TokenKind.LogicalAnd,            "and"        , 0, 0);
    public static readonly Token Or                      = new Token ( TokenKind.LogicalOr,             "or"         , 0, 0);
    public static readonly Token Xor                     = new Token ( TokenKind.LogicalXor,            "xor"        , 0, 0);
    public static readonly Token Repeat                  = new Token ( TokenKind.Repeat,                "repeat"     , 0, 0);
    public static readonly Token With                    = new Token ( TokenKind.With,                  "with"       , 0, 0);
    public static readonly Token Div                     = new Token ( TokenKind.Div,                   "div"        , 0, 0);
    public static readonly Token Mod                     = new Token ( TokenKind.Mod,                   "mod"        , 0, 0);
    public static readonly Token Var                     = new Token ( TokenKind.Var,                   "var"        , 0, 0);
    public static readonly Token Globalvar               = new Token ( TokenKind.Globalvar,             "globalvar"  , 0, 0);
    public static readonly Token Switch                  = new Token ( TokenKind.Switch,                "switch"     , 0, 0);
    public static readonly Token Case                    = new Token ( TokenKind.Case,                  "case"       , 0, 0);
    public static readonly Token Default                 = new Token ( TokenKind.Default,               "default"    , 0, 0);
    public static readonly Token Exit                    = new Token ( TokenKind.Exit,                  "exit"       , 0, 0);
    public static readonly Token Begin                   = new Token ( TokenKind.OpeningCurlyBrace,     "begin"      , 0, 0);
    public static readonly Token End                     = new Token ( TokenKind.ClosingCurlyBrace,     "end"        , 0, 0);
    public static readonly Token Self                    = new Token ( TokenKind.Identifier,            "self"       , 0, 0);
    public static readonly Token Other                   = new Token ( TokenKind.Identifier,            "other"      , 0, 0);
    public static readonly Token All                     = new Token ( TokenKind.Identifier,            "all"        , 0, 0);
    public static readonly Token Noone                   = new Token ( TokenKind.Identifier,            "noone"      , 0, 0);
    public static readonly Token Global                  = new Token ( TokenKind.Identifier,            "global"     , 0, 0);
    public static readonly Token BitwiseComplement       = new Token ( TokenKind.BitwiseComplement,     "~"          , 0, 0);
    public static readonly Token Not                     = new Token ( TokenKind.Not,                   "!"          , 0, 0);
    public static readonly Token Inequality              = new Token ( TokenKind.Inequality,            "!="         , 0, 0);
    public static readonly Token BitwiseXor              = new Token ( TokenKind.BitwiseXor,            "^"          , 0, 0);
    public static readonly Token LogicalXor              = new Token ( TokenKind.LogicalXor,            "^^"         , 0, 0);
    public static readonly Token XorAssignment           = new Token ( TokenKind.XorAssignment,         "^="         , 0, 0);
    public static readonly Token BitwiseAnd              = new Token ( TokenKind.BitwiseAnd,            "&"          , 0, 0);
    public static readonly Token LogicalAnd              = new Token ( TokenKind.LogicalAnd,            "&&"         , 0, 0);
    public static readonly Token AndAssignment           = new Token ( TokenKind.AndAssignment,         "&="         , 0, 0);
    public static readonly Token Multiply                = new Token ( TokenKind.Multiply,              "*"          , 0, 0);
    public static readonly Token MultiplyAssignment      = new Token ( TokenKind.MultiplyAssignment,    "*="         , 0, 0);
    public static readonly Token OpeningParenthesis      = new Token ( TokenKind.OpeningParenthesis,    "("          , 0, 0);
    public static readonly Token ClosingParenthesis      = new Token ( TokenKind.ClosingParenthesis,    ")"          , 0, 0);
    public static readonly Token Minus                   = new Token ( TokenKind.Minus,                 "-"          , 0, 0);
    public static readonly Token SubtractionAssignment   = new Token ( TokenKind.SubtractionAssignment, "-="         , 0, 0);
    public static readonly Token Plus                    = new Token ( TokenKind.Plus,                  "+"          , 0, 0);
    public static readonly Token AdditionAssignment      = new Token ( TokenKind.AdditionAssignment,    "+="         , 0, 0);
    public static readonly Token Assignment              = new Token ( TokenKind.Assignment,            "="          , 0, 0);
    public static readonly Token Equality                = new Token ( TokenKind.Equality,              "=="         , 0, 0);
    public static readonly Token OpeningCurlyBrace       = new Token ( TokenKind.OpeningCurlyBrace,     "{"          , 0, 0);
    public static readonly Token ClosingCurlyBrace       = new Token ( TokenKind.ClosingCurlyBrace,     "}"          , 0, 0);
    public static readonly Token OpeningSquareBracket    = new Token ( TokenKind.OpeningSquareBracket,  "["          , 0, 0);
    public static readonly Token ClosingSquareBracket    = new Token ( TokenKind.ClosingSquareBracket,  "]"          , 0, 0);
    public static readonly Token BitwiseOr               = new Token ( TokenKind.BitwiseOr,             "|"          , 0, 0);
    public static readonly Token LogicalOr               = new Token ( TokenKind.LogicalOr,             "||"         , 0, 0);
    public static readonly Token OrAssignment            = new Token ( TokenKind.OrAssignment,          "|="         , 0, 0);
    public static readonly Token Colon                   = new Token ( TokenKind.Colon,                 ":"          , 0, 0);
    public static readonly Token Semicolon               = new Token ( TokenKind.Semicolon,             ";"          , 0, 0);
    public static readonly Token LessThan                = new Token ( TokenKind.LessThan,              "<"          , 0, 0);
    public static readonly Token LessThanOrEqual         = new Token ( TokenKind.LessThanOrEqual,       "<="         , 0, 0);
    public static readonly Token ShiftLeft               = new Token ( TokenKind.ShiftLeft,             "<<"         , 0, 0);
    public static readonly Token GreaterThan             = new Token ( TokenKind.GreaterThan,           ">"          , 0, 0);
    public static readonly Token GreaterThanOrEqual      = new Token ( TokenKind.GreaterThanOrEqual,    ">="         , 0, 0);
    public static readonly Token ShiftRight              = new Token ( TokenKind.ShiftRight,            ">>"         , 0, 0);
    public static readonly Token Comma                   = new Token ( TokenKind.Comma,                 ","          , 0, 0);
    public static readonly Token Dot                     = new Token ( TokenKind.Dot,                   "."          , 0, 0);
    public static readonly Token Divide                  = new Token ( TokenKind.Divide,                "/"          , 0, 0);
    public static readonly Token DivideAssignment        = new Token ( TokenKind.DivideAssignment,      "/="         , 0, 0);
    public static readonly Token Increment               = new Token ( TokenKind.Increment,             "++"         , 0, 0);
    public static readonly Token Decrement               = new Token ( TokenKind.Decrement,             "--"         , 0, 0);
    public static readonly Token Constructor             = new Token ( TokenKind.Constructor,           "constructor", 0, 0);
    public static readonly Token New                     = new Token ( TokenKind.New,                   "new"        , 0, 0);
    public static readonly Token Function                = new Token ( TokenKind.Function,              "function"   , 0, 0);
    public static readonly Token Enum                    = new Token ( TokenKind.Enum,                  "enum"       , 0, 0);
}

internal class TextReader(string text)
{
    private readonly string text = text;

    private int idx = 0;
    
    public char Read()
    {
        if (idx < text.Length)
        {
            return text[idx++];
        }
        return '\uFFFF';
    }
    
    public char? Peek()
    {
        return idx >= text.Length ? null : text[idx];
    }
}

/// <summary>
/// 
/// </summary>
internal class Lexer
{
    public static Dictionary<string, Token> Words;
    public char Peek = ' ';
    private const char eof = '\uFFFF';
    public List<Token> pushed = [];
    private List<char> chars = [];
    
    private int line = 1;
    private int col = 1;
    private int tokencol = 0;

    private readonly TextReader reader;

    public Lexer(string code)
    {
        reader = new TextReader(code);
    }
        
    public void PutBack(Token tok)
    {
        pushed.Add(tok);
    }
    
    public static readonly Regex White = new Regex(@"^\s$");
    public static readonly Regex Letter = new Regex("^[A-Z]$", RegexOptions.IgnoreCase);
    public static readonly Regex LetterOrDigit = new Regex(@"^[A-Z0-9]$", RegexOptions.IgnoreCase);
    public static readonly Regex Digit = new Regex(@"^\d$");
    
    public Token Scan()
    {
        if (pushed.Count > 0)
        {
            var tok = pushed[0];
            pushed = pushed.Skip(1).ToList();
            return tok;
        }
        var skipwhite = false;
        do
        {
            while (White.IsMatch(Peek.ToString()))
            {
                Peek = Readch();
            }
            
            if (Peek == eof) return Tokens.Eof;
            
            tokencol = col;
            // [A-Za-z_][A-Za-z0-9_]*
            if (Letter.IsMatch(Peek.ToString()) || Peek == '_')
            {
                string sb = "";
                do
                {
                    sb += Peek;
                    Peek = Readch();
                } while (LetterOrDigit.IsMatch(Peek.ToString()) || Peek == '_');
                var s = sb.ToString();
                // reserve keywords and constants. Return their values literally as tokens
                if (Words.ContainsKey(s)) return Words[s];
                Token t;
                switch (s)
                {
                    case "true":
                    t = new BooleanToken(true, line, tokencol);
                    break;
                    case "false":
                    t = new BooleanToken(false, line, tokencol);
                    break;
                    default:
                    t = new Token(TokenKind.Identifier, line, tokencol) { Lexeme = s };
                    break;
                }
                Words[s] = t;
                return t;
            }
            else if (Peek == '\"')
            {
                string sb = "";
                do
                {
                    Peek = Readch();
                    switch (Peek)
                    {
                        case eof:
                        case '\"':
                            break;
                        default:
                            sb += Peek;
                            break;
                    }
                } while (Peek != '\"' && Peek != eof);
                Peek = ' ';
                var val = sb;
                return new StringLiteralToken(val, val, line, tokencol);
            }
            else if (Peek == '\'')
            {
                string sb = "";
                do
                {
                    Peek = Readch();
                    switch (Peek)
                    {
                        case eof:
                        case '\'':
                            break;
                        default:
                            sb += Peek;
                            break;
                    }
                } while (Peek != '\'' && Peek != eof);
                Peek = ' ';
                var val = sb;
                return new StringLiteralToken(val, val, line, tokencol);
            }
            else if (Peek == '.' || Digit.IsMatch(Peek.ToString()))
            {
                // returns: a dot or a real. real = .d[.d]*|d[.d]*
                var d = 0.0;
                if (Peek == '.')
                {
                    Peek = Readch();
                    if (!Digit.IsMatch(Peek.ToString()))
                    {
                        return new Token(TokenKind.Dot, line, tokencol);
                    }
                    // Fall through to the fractional part: peek holds the first digit
                }
                else
                {
                    // integral part (lexeme begins with a digit)
                    do
                    {
                        d = d * 10 + float.Parse(Peek.ToString());
                        Peek = Readch();
                    } while (Digit.IsMatch(Peek.ToString()));
                }

                var p = 10.0;
                // fractional part
                while (Digit.IsMatch(Peek.ToString()) || Peek == '.')
                {
                    if (Peek != '.')
                    {
                        d += float.Parse(Peek.ToString()) / p;
                        p *= 10;
                    }
                    Peek = Readch();
                }
                return new RealToken((float)d, null, line, tokencol);
            }
            else if (Peek == '$')
            {
                Peek = Readch();
                var d = 0;
                while (Digit.IsMatch(Peek.ToString()) || (Peek >= 'a' && Peek <= 'f') || (Peek >= 'A' && Peek <= 'F'))
                {
                    var hex = "0123456789ABCDEF";
                    d = d * 16 + hex.IndexOf(Peek.ToString().ToUpper());
                    Peek = Readch();
                }
                return new RealToken(d, null, line, tokencol);
            }
            else if (Peek == '/')
            {
                Peek = Readch();
                switch (Peek)
                {
                    case '=':
                        Peek = Readch(); return new Token(TokenKind.DivideAssignment, line, tokencol);
                    case '/':
                        do Peek = Readch(); while (Peek != '\n' && Peek != '\r' && Peek != eof);
                        skipwhite = true;
                        break;
                    case '*':
                        do
                        {
                            Peek = Readch();
                            if (Peek == '*')
                            {
                                Peek = Readch();
                                if (Peek == '/')
                                {
                                    Peek = Readch();
                                    break;
                                }
                            }
                        } while (Peek != eof);
                        //error(Token.None, "End-of-file found, '*/' expected");
                        skipwhite = true;
                        break;
                    default:
                        return new Token(TokenKind.Divide, line, tokencol);
                }
            }
            else
            {
                switch (Peek)
                {
                    case '~': Peek = Readch(); return new Token(TokenKind.BitwiseComplement, line, tokencol);
                    case '(': Peek = Readch(); return new Token(TokenKind.OpeningParenthesis, line, tokencol);
                    case ')': Peek = Readch(); return new Token(TokenKind.ClosingParenthesis, line, tokencol);
                    case '{': Peek = Readch(); return new Token(TokenKind.OpeningCurlyBrace, line, tokencol);
                    case '}': Peek = Readch(); return new Token(TokenKind.ClosingCurlyBrace, line, tokencol);
                    case '[': Peek = Readch(); return new Token(TokenKind.OpeningSquareBracket, line, tokencol);
                    case ']': Peek = Readch(); return new Token(TokenKind.ClosingSquareBracket, line, tokencol);
                    case ';': Peek = Readch(); return new Token(TokenKind.Semicolon, line, tokencol);
                    case ',': Peek = Readch(); return new Token(TokenKind.Comma, line, tokencol);
                    case ':': Peek = Readch(); if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.Assignment, ":=", line, tokencol); } return new Token(TokenKind.Colon, line, tokencol);
                    case '=': Peek = Readch(); if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.Equality, line, tokencol); } return new Token(TokenKind.Assignment, "=", line, tokencol);
                    case '!': Peek = Readch(); if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.Inequality, line, tokencol); } return new Token(TokenKind.Not, line, tokencol);
                    case '*': Peek = Readch(); if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.MultiplyAssignment, line, tokencol); } return new Token(TokenKind.Multiply, line, tokencol);
                    case '+': Peek = Readch();
                    if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.AdditionAssignment, line, tokencol); }
                    if (Peek == '+') { Peek = Readch(); return new Token(TokenKind.Increment, line, tokencol); }
                    return new Token(TokenKind.Plus, line, tokencol);
                    case '-': Peek = Readch();
                    if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.SubtractionAssignment, line, tokencol); }
                    if (Peek == '-') { Peek = Readch(); return new Token(TokenKind.Decrement, line, tokencol); }
                    return new Token(TokenKind.Minus, line, tokencol);
                    case '&': Peek = Readch(); if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.AndAssignment, line, tokencol); }
                        else
                            if (Peek == '&') { Peek = Readch(); return new Token(TokenKind.LogicalAnd, line, tokencol); } return new Token(TokenKind.BitwiseAnd, line, tokencol);
                    case '|': Peek = Readch(); if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.OrAssignment, line, tokencol); }
                        else
                            if (Peek == '|') { Peek = Readch(); return new Token(TokenKind.LogicalOr, line, tokencol); } return new Token(TokenKind.BitwiseOr, line, tokencol);
                    case '^': Peek = Readch(); if (Peek == '=') { Peek = Readch(); return new Token(TokenKind.XorAssignment, line, tokencol); }
                        else
                            if (Peek == '^') { Peek = Readch(); return new Token(TokenKind.LogicalXor, line, tokencol); } return new Token(TokenKind.BitwiseXor, line, tokencol);
                    case '<':
                        Peek = Readch();
                        if (Peek == '=')
                        {
                            Peek = Readch();
                            return new Token(TokenKind.LessThanOrEqual, line, tokencol);
                        }
                        else if (Peek == '<')
                        {
                            Peek = Readch();
                            return new Token(TokenKind.ShiftLeft, line, tokencol);
                        }
                        else if (Peek == '>')
                        {
                            Peek = Readch();
                            return new Token(TokenKind.Inequality, "<>", line, tokencol);
                        }
                        else return new Token(TokenKind.LessThan, line, tokencol);
                    case '>':
                        Peek = Readch();
                        if (Peek == '=')
                        {
                            Peek = Readch();
                            return new Token(TokenKind.GreaterThanOrEqual, line, tokencol);
                        }
                        else if (Peek == '>')
                        {
                            Peek = Readch();
                            return new Token(TokenKind.ShiftRight, line, tokencol);
                        }
                        else return new Token(TokenKind.GreaterThan, line, tokencol);
                    default:
                        throw new Exception($"Unexpected symbol at line {line} column {tokencol}", null);
                }
            }
        } while (skipwhite);
        return Tokens.Eof;
    }
    
    public char Readch() {
        if (chars.Count > 0)
        {
            char chr = chars[0];
            chars = chars.Skip(1).ToList();
            return chr;
        }
        if (reader.Peek() == -1) return eof;
        var c = reader.Read();
        col++;
        if (c == '\n') {
                line++;
            col = 1;
        }
        return c;
    }

    public char Peekch() {
        var c = Readch();
        Putbackc(c);
        return c;
    }

    public void Putbackc(char c)
    {
        chars.Add(c);
    }
    
    static Lexer () {
        Words = [];
        
        Reserve(Tokens.Break);
        Reserve(Tokens.Continue);
        Reserve(Tokens.Return);
        Reserve(Tokens.If);
        Reserve(Tokens.Then);
        Reserve(Tokens.Else);
        Reserve(Tokens.While);
        Reserve(Tokens.Do);
        Reserve(Tokens.Until);
        Reserve(Tokens.For);
        Reserve(Tokens.NotWord);
        Reserve(Tokens.And);
        Reserve(Tokens.Or);
        Reserve(Tokens.Xor);
        Reserve(Tokens.Repeat);
        Reserve(Tokens.With);
        Reserve(Tokens.Div);
        Reserve(Tokens.Mod);
        Reserve(Tokens.Var);
        Reserve(Tokens.Globalvar);
        Reserve(Tokens.Switch);
        Reserve(Tokens.Case);
        Reserve(Tokens.Default);
        Reserve(Tokens.Exit);
        Reserve(Tokens.Begin);
        Reserve(Tokens.End);
        Reserve(Tokens.Self);
        Reserve(Tokens.Other);
        Reserve(Tokens.All);
        Reserve(Tokens.Noone);
        Reserve(Tokens.Global);
        Reserve(Tokens.Constructor);
        Reserve(Tokens.New);
        Reserve(Tokens.Function);
        Reserve(Tokens.Enum);
    }
    
    private static void Reserve(Token t)
    {
        Words[t.Lexeme ?? "Null"] = t;
    }
}
