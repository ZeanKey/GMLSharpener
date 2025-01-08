#define FEATURE_UNDEFINED_STRUCT_MEMBER_ACCESSIBLE
#define DEBUGGER_FUNCTION_INVOKE_INFO

using GMLib;

using System.Linq.Expressions;
using System.Reflection;
using System.Diagnostics.CodeAnalysis;
using System.Dynamic;
using System.Collections;
using System.Diagnostics;
using GMLSharpener.GM;

namespace GMLSharpener;

/*
/// <summary>
/// Statement nodes.
/// </summary>
internal enum StatementKind
{
    AdditionAssignment,
    AndAssignment,
    Assignment,
    Break,
    Call,
    CallStatement,
    Case,
    Continue,
    Default,
    DivideAssignment,
    Do,
    Exit,
    For,
    Globalvar,
    If,
    MultiplyAssignment,
    OrAssignment,
    Repeat,
    Return,
    Sequence,
    SubtractionAssignment,
    Switch,
    Var,
    While,
    With,
    XorAssignment,
    Nop
}

/// <summary>
/// Expression nodes.
/// </summary>
internal enum ExpressionKind
{
    None,
    Access,
    Addition,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Call,
    Complement,
    Constant,
    Div,
    Divide,
    Equality,
    GreaterThan,
    GreaterThanOrEqual,
    Grouping,
    Id,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    LogicalOr,
    LogicalXor,
    Minus,
    Mod,
    Multiply,
    Not,
    NotEqual,
    Plus,
    ShiftLeft,
    ShiftRight,
    Subtraction
}
*/

/// <summary>
/// Parse the GML code string into Linq Expression Tree.
/// </summary>
public partial class ExpressionParser;

// Tools & Reflections
public partial class ExpressionParser
{
    internal static readonly MethodInfo Print = typeof(GML).GetMethod("show_debug_message") ?? throw new Exception("Debugger doesn't exists"); // Debugger, can be inserted into the Expression to print certain value.

    /// <summary>
    /// Invoke C# Method of GameMaker Callable with value boxed in GameMaker Value struct.
    /// GameMaker Value I/O repacked. 
    /// This method is <b>not</b> supposed to be called by user, the only reason it's public is because otherwise it will be discarded by compiler.
    /// </summary>
    /// <param name="callable">     The callable method which must be either C# MethodInfo or GameMaker Method/Script/Function</param>
    /// <param name="arguments">    The argument array of GameMaker boxed value</param>
    /// <returns>The return value of target method, null if no return value exists</returns>
    /// <exception cref="Exception">The type of the callable is nether MethodInfo or Method</exception>
    public static RValue InvokeWithGMValue(object caller, object callable, params RValue[] arguments)
    {
        MethodInfo method;
        object? self;
        if (callable is MethodInfo methodInfo)
        {
            self = null;
            method = methodInfo;
        }
        else if (callable is Method msd)
        {
            self = msd;
            method = GMMethodInvoker;
        }
        else if (callable is Asset.Script script)
        {
            self = new Method(caller, script) { Name = script.Name };
            method = GMMethodInvoker;
        }
        else if (callable is Asset.Script.Function function)
        {
            self = new Method(caller, function);
            method = GMMethodInvoker;
        }
        else throw new Exception("Trying to invoke a value isn't callable");
        // Unbox value from the GMValue type
        // If params attribute is being assigned, pack the remains into Array<T>
        // The type identifying depends on DLR
        List<object?> args = [];
        var argInfos = method.GetParameters();
        for (int i = 0; i < arguments.Length; i++)
        {
            if (argInfos[i].IsDefined(typeof(ParamArrayAttribute), false))
            {
                var paramType = argInfos[i].ParameterType.GetElementType() ?? throw new Exception("Using params keyword with type not collection.");
                var paramArgs = Array.CreateInstance(paramType, arguments.Length - i);
                for (int j = i; j < arguments.Length; j ++)
                {
                    dynamic? arg = arguments[j].Value;
                    paramArgs.SetValue(arg, j - i);
                }
                args.Add(paramArgs);
                break;
            }
            var rvalue = arguments[i];
            args.Add(Convert.ChangeType(rvalue.Value, argInfos[i].GetType()));
        }
        if (args.Count == 0)
        {
            return new RValue(method.Invoke(self, [null]));
        }
        else return new RValue(method.Invoke(self, [.. args]));
    }

#region Expression Tree Reflections
    // Methods
    private static readonly MethodInfo GMInvoker = typeof(ExpressionParser).GetMethod("InvokeWithGMValue") ?? throw new Exception("GM Value Invoker doesn't exists");
    private static readonly MethodInfo GMMethodInvoker = typeof(Method).GetMethod("Invoke") ?? throw new Exception("Method Invoker doesn't exists");
    private static readonly MethodInfo GMScriptGetter = typeof(ExpressionParser).GetMethod("ResolveScriptHandle") ?? throw new Exception("Script Getter doesn't exists");

    // Ctors
    private static readonly ConstructorInfo GMValueConstructor = typeof(RValue).GetConstructor([typeof(object)]) ?? throw new Exception();
    private static readonly ConstructorInfo GMMethodConstructor = typeof(Method).GetConstructor([typeof(object), typeof(Asset.Script)]) ?? throw new Exception();
    private static readonly ConstructorInfo GMStructConstructor = typeof(ExpandoObject).GetConstructor(Type.EmptyTypes) ?? throw new Exception("");

    /// Accessors
    private static readonly PropertyInfo GMValueIndexer = typeof(RValue).GetProperties().First(x => x.GetIndexParameters().Select(y => y.ParameterType).SequenceEqual([typeof(float[])])) ?? throw new Exception("RValue missing indexer.");
    private static readonly PropertyInfo GMValueMemberAccessor = typeof(RValue).GetProperties().First(x => x.GetIndexParameters().Select(y => y.ParameterType).SequenceEqual([typeof(string)])) ?? throw new Exception("RValue missing member accessor.");
#endregion

#region Expression Tree Nodes
    /// <summary>
    /// Combine the GameMaker Value params, C# object?[]? params, and default value assignments into function parameter initializer.
    /// </summary>
    /// <param name="parameters">GameMaker Value type parameters' parameter expressions</param>
    /// <param name="arguments">C# parameters's parameter expression as object?[]?</param>
    /// <param name="initializer">Default value assignment</param>
    /// <returns>Expression to put in function body</returns>
    private static Expression GMArgumentInitializer(ParameterExpression[] parameters, ParameterExpression arguments, Expression initializer)
    {
        var exprs = new List<Expression> { initializer };
        for (int i = 0; i < parameters.Length; i++)
        {
            exprs.Add(Expression.Assign
            (
                parameters[i], Expression.New(GMValueConstructor, Expression.ArrayAccess(arguments, Expression.Constant(i)))
            ));
        }
        return Expression.Block(exprs.ToArray());
    }

    /// <summary>
    /// Create a expression will invoke the method in C# params with GameMaker Value params in run.
    /// This method collaborates with <b>InvokeWithGMValue</b>.
    /// </summary>
    /// <param name="method">The callable method expression which must be either C# MethodInfo or GameMaker Method</param>
    /// <param name="arguments">The argument array of GameMaker boxed value</param>
    /// <returns>Expression calling C# method with GameMaker Values</returns>
    private static Expression GMCall(Expression caller, Expression method, Expression[] arguments)
    {
        Expression self = Expression.Field(caller, "Value");
        Expression function = Expression.Field(method, "Value");
        return Expression.Call(null, GMInvoker, [Expression.Convert(self, typeof(object)), Expression.Convert(function, typeof(object)), Expression.NewArrayInit(typeof(RValue), arguments)]);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="instance"></param>
    /// <param name="member"></param>
    /// <returns></returns>
    private static Expression GMMember(Expression instance, Expression member)
    {
        return Expression.Property(instance, GMValueMemberAccessor, member);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="array"></param>
    /// <param name="indexes"></param>
    /// <returns></returns>
    private static Expression GMIndexer(Expression array, Expression[] indexes)
    {
        List<Expression> args = [];
        foreach (var index in indexes)
        {
            args.Add(Expression.Convert(Expression.Field(index, "Value"), typeof(float)));
        }
        return Expression.Property(array, GMValueIndexer, [Expression.NewArrayInit(typeof(float), args)]);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="original"></param>
    /// <returns></returns>
    private static Expression ToGMValue(Expression original)
    {
        if (original.Type == typeof(RValue))
        {
            return original;
        }
        return Expression.New(GMValueConstructor, Expression.Convert(original, typeof(object)));
    }
#endregion

    /// <summary>
    /// Get the script through handle.
    /// This method is <b>not</b> supposed to be called by user, the only reason it's public is because otherwise it will be discarded by compiler.
    /// </summary>
    /// <param name="handle">The handle of the assets visiting</param>
    /// <returns>The script handle pointing at</returns>
    /// <exception cref="Exception">Script doesn't exists</exception>
    public static Asset.Script ResolveScriptHandle(Handle<Asset.Script> handle)
    {
        if (!GML.ScriptManager.TryFindAsset(handle, out var asset)) throw new Exception();
        return asset!;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="name"></param>
    /// <returns></returns>
    /// <exception cref="Exception"></exception>
    private Expression? FindIdentifierExpression(string name)
    {
        if (name == "self")
        {
            return Expression.New(GMValueConstructor, SelfStack.Peek());
        }
        if (name == "other")
        {
            throw new NotImplementedException();
            // TODO
            // return Expression.New(GMValueConstructor, OtherStack.Peek());
        }
        if (name == "noone")
        {
            throw new Exception();
        }
        if (name == "all")
        {
            throw new Exception();
        }
        if (name == "global")
        {
            
        }

        foreach (var context in BlockScopeStack)
        {
            if (context.TryGetValue(name, out ParameterExpression? val))
            {
                return val;
            }
        }
        if (typeof(GML).GetMethod(name) is MethodInfo runtimeFunction)
        {
            return Expression.Constant(new RValue() { Value = runtimeFunction });
        }
        if (GML.ScriptManager.TryFindAssetHandle(name, out Handle<Asset.Script> handle) && GML.ScriptManager.TryFindAsset(handle, out _))
        {
            return Expression.New
            (
                GMValueConstructor,
                Expression.Call(null, GMScriptGetter, Expression.Constant(handle))
            );
        }
        return GMMember(Expression.New(GMValueConstructor, SelfStack.Peek()), Expression.Constant(name));
    }
}

// Exposed
public partial class ExpressionParser
{
    /// <summary>
    /// Stores break - continue pair for the statements can be controlled by these keywords, like do/for/while/switch statements.
    /// </summary>
    private readonly struct StatementLabelPair
    {
        public readonly LabelTarget Begin;
        public readonly LabelTarget End;

        public StatementLabelPair()
        {
            Begin = Expression.Label();
            End = Expression.Label();
        }
    }

    /// <summary>
    /// 
    /// </summary>
    private readonly struct HoistedEnum
    {
        public readonly string Name;
        public readonly string[] Members;
        public HoistedEnum(string name, IEnumerable<string> members)
        {
            Name = name;
            Members = members.ToArray();
        }
    }

#region Custom Node
    /// <summary>
    /// This expression won't exist in exported expression tree,
    /// mainly designed as the ret for first compiling, detecting the function statements' header.
    /// </summary>
    internal class FunctionHeaderExpression : Expression
    {
        public string Name;
        public FunctionHeaderExpression(string name)
        {
            Name = name;
        }
    }
#endregion

    // The global object binded to the parser
    private readonly IDictionary<string, object?> Global;
    // Stores tokens for script compiling
    private Stack<Token> TokenStack;

#region Compile Context
    private Stack<StatementLabelPair> StatementLabelStack; // Stores break - continue pairs for the statements can be controlled by these keywords, like do/for/while/switch statements
    private Stack<Dictionary<string, ParameterExpression>> BlockScopeStack;
    private Stack<Dictionary<string, Expression<Asset.Script.Function>>> BlockFunctionBodyStack; // Stores the function definations and push them to the top of the block / script to implement the function raise
    private Stack<Expression> SelfStack; // For condition like self keyword in function definations or with statements where the self context changes
    private Stack<LabelTarget> ReturnStack; // For condition like return keyword in function definations where the ret context changes

    private Dictionary<string, ParameterExpression> GlobalScope; // Stores global variables declaration for script compiling
    private bool IsFirstCompile; // Statement controlling the compile function on pre-compile statements and declarations.
#endregion

#pragma warning disable CS8618
    public ExpressionParser(IDictionary<string, object?> global)
    {
        Global = global;
    }
#pragma warning restore CS8618

    private void Init(string code)
    {
        // Initialize the contexts
        TokenStack             = [];
        BlockScopeStack        = [];
        StatementLabelStack    = [];
        SelfStack              = [];
        ReturnStack            = [];
        BlockFunctionBodyStack = [];
        GlobalScope            = [];
        
        // Lexer trans the raw code into token stack
        Lexer lexer = new(code);
        Token? token;
        do
        {
            token = lexer.Scan();
            if (token is not null) TokenStack.Push(token);
        }
        while (token is null || token.Type != TokenType.Eof);
        TokenStack = new(TokenStack);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="code"></param>
    /// <returns></returns>
    public LambdaExpression ParseObjectEvent(string code)
    {
        // Initialize token stack
        code = "{" + code + "}"; // Object event script is considered as a function rather than an actual script asset, therfore, it should under an isolate scope
        Init(code);

        // Initialize context
        var paramSelf   = Expression.Parameter(typeof(Instance), "EVENT_LAMBDA_CALLER");
        var paramOther  = Expression.Parameter(typeof(Instance), "EVENT_LAMBDA_OTHER"); // Trash
        SelfStack.Push(paramSelf); ReturnStack.Push(Expression.Label());

        // Catch script body
        List<Expression> body = [];
        while (TokenStack.Peek().Type != TokenType.Eof)
        {
            body.Add(GetStatementLine());
        }
        body.Add(Expression.Label(ReturnStack.Pop())); // Return label, here is for exiting the event, return value won't affect following procedure.
        
        return Expression.Lambda<Action<Instance?, Instance?>>(Expression.Block(body), [paramSelf, paramOther]);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="code"></param>
    /// <returns></returns>
    public LambdaExpression ParseScript(string code)
    {
        // Initialize token stack
        code = "{" + code + "}"; // Same to object event, formal GameMaker Script assest consider as an indiviual function after compiled and will be called by ScriptPerform.
        Init(code);

        // Initialize context
        SelfStack.Push(Expression.Constant(Global)); ReturnStack.Push(Expression.Label());
        
        // Catch script body
        List<Expression> body = [];
        while (TokenStack.Peek().Type != TokenType.Eof)
        {
            body.Add(GetStatementLine());
        }
        body.Add(Expression.Label(ReturnStack.Pop()));
        
        return Expression.Lambda<Action>(Expression.Block(body));
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="code"></param>
    /// <returns></returns>
    /// <exception cref="NotImplementedException"></exception>
    public LambdaExpression ParseLambda(string code)
    {
        throw new NotImplementedException();
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="code"></param>
    public void ParseExportScriptPreCompile(string code)
    {
        // Initialize token stack
        Init(code);
        
        var headers = GetFunctionHeaders();
        foreach (var header in headers)
        {
            var script = new Asset.Script(static (_, _) => {}, header);
            GML.ScriptManager.Register(script);
        } 
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="code"></param>
    public void ParseExportScript(string code)
    {
        // Initialize token stack
        Init(code);

        // Catch function headers
        var headers = GetFunctionHeaders();

        // Catch function definitions
        BlockFunctionBodyStack.Push([]);
        while (TokenStack.Peek().Type != TokenType.Eof)
        {
            GetStatementLine();
        }
        var lambdas = BlockFunctionBodyStack.Pop();
        
        // Register functions
        foreach (var header in headers)
        {
            var script = new Asset.Script(lambdas[header].Compile(), header);
            GML.ScriptManager.Override(script);
        }
    }
}

// Main Parser
public partial class ExpressionParser
{
    private bool TryPopToken(TokenType type, out Token? token)
    {
        if (TokenStack.Peek().Type == type)
        {
            token = TokenStack.Pop();
            return true;
        }
        token = null;
        return false;
    }
    
    /// <summary>
    /// 
    /// </summary>
    /// <param name="left"></param>
    /// <param name="right"></param>
    /// <returns></returns>
    /// <exception cref="Exception"></exception>
    private Expression PopNestedPair(TokenType left, TokenType right)
    {
        var counter = 1;
        if (!TryPopToken(left, out var _)) throw new Exception();
        while (counter > 0)
        {
            var token = TokenStack.Pop();
            if (token.Type == left)
                counter++;
            else if (token.Type == right)
                counter--;
        }
        return Expression.Empty();
    }

    private Expression PopExpression()
    {
        return PopLogical();
    }

    private Expression PopLogical()
    {
        var val = PopRelational();
        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenType.LogicalAnd:
                TokenStack.Pop();
                val = Expression.And(val, PopRelational());
                continue;
                
                case TokenType.LogicalOr:
                TokenStack.Pop();
                val = Expression.Or(val, PopRelational());
                
                continue;
                case TokenType.LogicalXor:
                TokenStack.Pop();
                val = Expression.ExclusiveOr(val, PopRelational());
                continue;
                
                default:
                return val;
            }
        }
    }

    private Expression PopRelational()
    {
        var val = PopBitwise();
        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenType.LessThan:
                TokenStack.Pop();
                val = ToGMValue(Expression.LessThan(val, PopBitwise()));
                continue;

                case TokenType.LessThanOrEqual:
                TokenStack.Pop();
                val = ToGMValue(Expression.LessThanOrEqual(val, PopBitwise()));
                continue;

                case TokenType.GreaterThan:
                TokenStack.Pop();
                val = ToGMValue(Expression.GreaterThan(val, PopBitwise()));
                continue;

                case TokenType.GreaterThanOrEqual:
                TokenStack.Pop();
                val = ToGMValue(Expression.GreaterThanOrEqual(val, PopBitwise()));
                continue;
                case TokenType.Inequality:

                TokenStack.Pop();
                val = ToGMValue(Expression.NotEqual(val, PopBitwise()));
                continue;

                case TokenType.Equality:
                case TokenType.Assignment:
                TokenStack.Pop();
                val = ToGMValue(Expression.Equal(val, PopBitwise()));
                continue;

                default:
                return val;
            }
        }
    }

    private Expression PopBitwise()
    {
        var val = PopShift();
        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenType.BitwiseAnd:
                TokenStack.Pop();
                val = Expression.And(val, PopRelational());
                continue;
                
                case TokenType.BitwiseOr:
                TokenStack.Pop();
                val = Expression.Or(val, PopRelational());
                
                continue;
                case TokenType.BitwiseXor:
                TokenStack.Pop();
                val = Expression.ExclusiveOr(val, PopRelational());
                continue;
                
                default:
                return val;
            }
        }
    }

    private Expression PopShift()
    {
        var val = PopAdditive();
        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenType.ShiftLeft:
                TokenStack.Pop();
                val = Expression.LeftShift(val, PopAdditive());
                continue;
                
                case TokenType.ShiftRight:
                TokenStack.Pop();
                val = Expression.RightShift(val, PopAdditive());
                continue;

                default:
                return val;
            }
        }
    }

    private Expression PopAdditive()
    {
        
        var val = PopTerm();
        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenType.Plus:
                TokenStack.Pop();
                val = Expression.Add(val, PopTerm());
                continue;
                
                case TokenType.Minus:
                TokenStack.Pop();
                val = Expression.Subtract(val, PopTerm());
                continue;

                default:
                return val;
            }
        }
    }

    private Expression PopTerm()
    {
        var val = PopPrefix();
        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenType.Multiply:
                TokenStack.Pop();
                val = Expression.Multiply(val, PopPrefix());
                continue;

                case TokenType.Divide:
                TokenStack.Pop();
                val = Expression.Divide(val, PopPrefix());
                continue;

                case TokenType.Mod:
                TokenStack.Pop();
                val = Expression.Modulo(val, PopPrefix());
                continue;

                case TokenType.Div:
                TokenStack.Pop();
                val = Expression.Divide(ToGMValue(Expression.Convert(Expression.Field(val, "Value"), typeof(int))), PopPrefix());
                continue;

                default:
                return val;
            }
        }
    }

    private Expression PopPrefix()
    {
        switch (TokenStack.Peek().Type)
        {
            case TokenType.Plus:
            TokenStack.Pop();
            return PopPrefix();

            case TokenType.Minus:
            TokenStack.Pop();
            return Expression.Negate(PopPrefix());

            case TokenType.Not:
            TokenStack.Pop();
            return Expression.Not(PopPrefix());
            
            case TokenType.BitwiseComplement:
            TokenStack.Pop();
            return Expression.Not(PopPrefix());
            
            case TokenType.Increment:
            TokenStack.Pop();
            return Expression.AddAssign(PopPrefix(), Expression.Constant(new RValue(1f), typeof(RValue)));

            case TokenType.Decrement:
            TokenStack.Pop();
            return Expression.SubtractAssign(PopPrefix(), Expression.Constant(new RValue(1f), typeof(RValue)));

            default:
            return PopPostfix();
        }
    }

    private Expression PopPostfix()
    {
        var val = PopAccess();
        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenType.Increment:
                {
                    TokenStack.Pop();
                    ParameterExpression temp = Expression.Variable(typeof(RValue));
                    val = Expression.Block
                    (
                        [temp],
                        Expression.Assign(temp, val),
                        Expression.AddAssign(val, Expression.Constant(new RValue(1f), typeof(RValue))),
                        temp
                    );
                }
                break;
                
                case TokenType.Decrement:
                {
                    TokenStack.Pop();
                    ParameterExpression temp = Expression.Variable(typeof(RValue));
                    val = Expression.Block
                    (
                        [temp],
                        Expression.Assign(temp, val),
                        Expression.SubtractAssign(val, Expression.Constant(new RValue(1f), typeof(RValue))),
                        temp
                    );
                }
                break;
                
                default:
                return val;
            }
        }
    }

    /// <summary>
    /// Capture and pop next access, new or call expression.
    /// </summary>
    /// <returns></returns>
    /// <exception cref="Exception"></exception>
    private Expression PopAccess()
    {
        // Detect new keyword
        bool isNew = TryPopToken(TokenType.New, out _); // If new is detected, capture the first method caller as ctor
        bool isCtorCaptured = false;

        // The call and access is combined as they can appear together as A = A() | A.B | I[B]
        Expression accessed = Expression.New(GMValueConstructor, Expression.Constant(null, typeof(object)));
        Expression val = PopFactor();

        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenType.Dot:
                accessed = val;
                TokenStack.Pop();
                if (!TryPopToken(TokenType.Identifier, out Token? member))
                {
                    throw new Exception($"Expected Member name, got {member?.Type}.");
                }
                val = GMMember(val, Expression.Constant(member!.Value!));
                break;
                
                case TokenType.OpeningSquareBracket:
                val = GMIndexer(val, PopIndices());
                break;

                case TokenType.OpeningParenthesis:
                var args = PopArguments();
                if (isNew && !isCtorCaptured)
                {
                    var caller = Expression.Variable(typeof(RValue));
                    val = Expression.Block
                    (
                        [caller],
                        Expression.Assign(caller, ToGMValue(Expression.New(GMStructConstructor))),
                        GMCall(caller, val, args),
                        caller
                    );
                    isCtorCaptured = true;
                }
                else val = GMCall(accessed, val, args);
                break;

                default:
                if (isNew && !isCtorCaptured) throw new Exception("Expects ctor after new keyword.");
                return val;
            }
        }
    }

#region Tools
    // This is not a well design, the method is severely context depended.
    /// <summary>
    /// Pop the parameters (argument definition) parenthesis inclusive.
    /// </summary>
    /// <remarks>
    /// If the parameter have default value, they'll be captured and trans to assign expression.
    /// Also, the parameter will be automatically pushed into the current block scope, as parse the dictionary as return value is a waste. So make sure you've pushed the block scope to catch the definitions.
    /// </remarks>
    /// <param name="initializer"></param>
    /// <exception cref="Exception"></exception>
    private void PopParameters(out Expression initializer)
    {
        List<Expression> inits = [];
        if (!TryPopToken(TokenType.OpeningParenthesis, out _)) throw new Exception("Expected closing parenthesis for callable invokement.");
        while (TokenStack.Peek().Type == TokenType.Identifier)
        {
            var token = TokenStack.Pop();
            var name = token.Value ?? "";
            var variable = Expression.Variable(typeof(RValue), name);
            BlockScopeStack.Peek().Add(name, variable);
            if (TokenStack.Peek().Type == TokenType.Assignment)
            {
                TokenStack.Pop();
                inits.Add(Expression.Assign(variable, PopExpression()));
            }
            if (TokenStack.Peek().Type == TokenType.Comma) TokenStack.Pop();
        }
        if (!TryPopToken(TokenType.ClosingParenthesis, out _)) throw new Exception("Expected closing parenthesis for callable invokement.");
        if (inits.Count == 0) initializer = Expression.Empty();
        else initializer = Expression.Block(inits);
    }

    private Expression[] PopArguments()
    {
        if (IsFirstCompile)
        {
            PopNestedPair(TokenType.OpeningParenthesis, TokenType.ClosingParenthesis);
            return [];
        }
        
        if (!TryPopToken(TokenType.OpeningParenthesis, out _))
        {
            throw new Exception("Expected closing parenthesis for callable invokement.");
        }

        List<Expression> arguments = [];
        var next = TokenStack.Peek();
        if (next.Type != TokenType.ClosingParenthesis && next.Type != TokenType.Eof)
        {
            arguments.Add(PopExpression());
        }
        while (TokenStack.Peek().Type == TokenType.Comma)
        {
            TokenStack.Pop();
            arguments.Add(PopExpression());
        }
        if (!TryPopToken(TokenType.ClosingParenthesis, out _))
        {
            throw new Exception("Expected closing parenthesis for callable invokement.");
        }
        return [..arguments];
    }

    private Expression[] PopIndices()
    {
        if (TokenStack.Peek().Type != TokenType.OpeningSquareBracket)
            throw new Exception($"Expected Closing Square Bracket, got {TokenStack.Peek().Type}");
        TokenStack.Pop();

        List<Expression> indices = [];
        while (TokenStack.Peek().Type != TokenType.ClosingSquareBracket)
        {
            indices.Add(PopExpression());
            var next = TokenStack.Peek();
            if (next.Type == TokenType.Comma)
                TokenStack.Pop();
            else if (next.Type != TokenType.ClosingSquareBracket)
                throw new Exception($"Expected Closing Square Bracket, got {next.Type}.");
        }
        TokenStack.Pop();

        if (indices.Count > 2)
            throw new Exception("Multi-dimentional array is not supported in GameMaker 8");
        
        return [.. indices];
    }

    private FunctionHeaderExpression? PopFunctionHeader()
    {
        TokenStack.Pop();
        // Try pop name
        TryPopToken(TokenType.Identifier, out var tokenName); string? name = tokenName?.Value;
        // Try pop arguments
        BlockScopeStack.Push([]); PopParameters(out _); BlockScopeStack.Pop();
        // Try pop base constructor
        if (TryPopToken(TokenType.Colon, out _))
        {
            if (!TryPopToken(TokenType.Identifier, out _)) throw new Exception($"Expected identifier, got {name}.");
            PopParameters(out _);
        }
        TryPopToken(TokenType.Constructor, out _);
        // Pop function body.
        PopBlock();
        if (name is not null) return new FunctionHeaderExpression(name);
        return null;
    }

    private Expression<Asset.Script.Function> PopFunction(out string? name, Func<string?, Expression?> calleeParser)
    {
        TokenStack.Pop();
        // Try pop the name for the function.
        name = null;
        if (TryPopToken(TokenType.Identifier, out var tokenName)) name = tokenName!.Value ?? throw new Exception();
        
        // Function components
        bool isCtor = false; Expression? baseCtor = null;
        
        Dictionary<string, ParameterExpression> parameters = [];
        ParameterExpression caller = Expression.Parameter(typeof(object), "LAMBDA_CALLER"); // The object self keyword pointing to.
        ParameterExpression arguments = Expression.Parameter(typeof(object[]), "LAMBDA_ARGUMENTS");
        Expression body;
        Expression initializer;
        LabelTarget ret = Expression.Label(typeof(object));

        BlockScopeStack.Push(parameters); SelfStack.Push(caller); ReturnStack.Push(ret);
        {
            PopParameters(out initializer);
            // Try get base Constructor if it has.
            if (TryPopToken(TokenType.Colon, out _))
            {
                baseCtor = GMCall(ToGMValue(caller), PopFactor(), PopArguments());
                isCtor = true;
            }

            // Check if the function is a constructor.
            if (TryPopToken(TokenType.Constructor, out _))
            {
                isCtor = true;
            } // The exception will only be thrown when having base constructors without constructor keyword.
            else if (isCtor) throw new Exception("Expects Constructor keywords since base ctor is defined.");

            // Pop function body.
            if (name is null)
                BlockScopeStack.Push([]);
            else
            {
                var callee = calleeParser(name);
                if (callee is null) // cannot find callee
                    throw new Exception(); 
                if (callee is ParameterExpression calleeVariable) // callee is a local function
                    BlockScopeStack.Push(new()
                    {
                        { name!, calleeVariable }
                    });
                else // if callee is a script function, it won't need to be put into local context
                    BlockScopeStack.Push([]);
            }

            body = PopBlock();
            BlockScopeStack.Pop();

            if (isCtor && baseCtor is not null) body = Expression.Block([baseCtor!, body]); // Include base ctor if it has.
        }
        BlockScopeStack.Pop(); SelfStack.Pop(); ReturnStack.Pop();
        
        // Pack up
        ParameterExpression[] variables = [.. parameters.Values];
        List<Expression> expressions = 
        [
            GMArgumentInitializer(variables, arguments, initializer), // Initialize the parameters and assign the arguments
            body,
            Expression.Label(ret, Expression.Constant(null))
        ];
        body = Expression.Block
        (
            variables,
            expressions
        );

        return Expression.Lambda<Asset.Script.Function>(body, name, [caller, arguments]);
    }
#endregion

    private Expression PopFactor()
    {
        Expression expression;
        switch (TokenStack.Peek().Type)
        {
            // Number
            case TokenType.Real:
            return Expression.Constant(new RValue() { Value = ((RealToken)TokenStack.Pop()).Value }, typeof(RValue));
            // String
            case TokenType.StringLiteral:
            return Expression.Constant(new RValue() { Value = ((StringToken)TokenStack.Pop()).Value }, typeof(RValue));
            // Boolean
            case TokenType.Boolean:
            return Expression.Constant(new RValue() { Value = ((BooleanToken)TokenStack.Pop()).Value }, typeof(RValue));
            // Asset / Field / Variable
            case TokenType.Identifier:
            // Expectedly, this case will only be called for the first identifier
            // I'm not sure if it will be called out of such condition
            return FindIdentifierExpression(TokenStack.Pop().Value!) ?? throw new Exception("Unclaimed variable");
            // Group
            case TokenType.OpeningParenthesis:
            {
                TokenStack.Pop();
                expression = Expression.Block(PopExpression());
                if (!TryPopToken(TokenType.ClosingParenthesis, out Token? _))
                    throw new Exception("Expected Closing Parenthesis.");
                return expression;
            }
            // Struct
            case TokenType.OpeningCurlyBrace:
            {
                TokenStack.Pop();
                List<Expression> initializer = [];
                var inst = Expression.Variable(typeof(RValue));
                SelfStack.Push(inst); BlockScopeStack.Push([]);
                {
                    initializer.Add(Expression.Assign(inst, ToGMValue(Expression.New(GMStructConstructor))));
                    while (TryPopToken(TokenType.Identifier, out var key))
                    {
                        if (TryPopToken(TokenType.Colon, out _))
                        {
                            initializer.Add(Expression.Assign(GMMember(inst, Expression.Constant(key!.Value)), PopExpression()));
                        }
                        else throw new Exception("Expects value assignment for struct.");
                        if (!TryPopToken(TokenType.Comma, out _)) break;
                    }
                    if (!TryPopToken(TokenType.ClosingCurlyBrace, out _)) throw new Exception("Expects Closing Curly Brace for struct declearation.");
                    initializer.Add(inst);
                }
                SelfStack.Pop(); BlockScopeStack.Pop();
                return Expression.Block([inst], initializer);
            }
            // Function
            case TokenType.Function:
            {  
                var callee  = Expression.Parameter(typeof(RValue), "LAMBDA_CALLEE");
                var func    = PopFunction(out var name, (_) => callee);
                
                if (name is not null)
                    return Expression.Block([callee], [Expression.Assign(callee, ToGMValue(func)), callee]);
                else
                    return Expression.Block(ToGMValue(func));
            }

            default:
            throw new Exception();
        }
    }

    private Expression PopBlock()
    {
        if (IsFirstCompile)
        {
            PopNestedPair(TokenType.OpeningCurlyBrace, TokenType.ClosingCurlyBrace);
            return Expression.Empty();
        }

        // Capture function headers
        Dictionary<string, ParameterExpression> funcs = [];
        var headers = GetFunctionHeaders();
        foreach (var header in headers)
        {
            funcs.Add(header, Expression.Parameter(typeof(RValue)));
        }

        // Push block scope
        BlockScopeStack.Push(funcs); BlockFunctionBodyStack.Push([]);
        // Capture body statements
        List<Expression> blockStmts = [];
        if (!TryPopToken(TokenType.OpeningCurlyBrace, out Token? _)) throw new Exception();
        while (TokenStack.Peek().Type != TokenType.ClosingCurlyBrace && TokenStack.Peek().Type != TokenType.Eof)
        {
            blockStmts.Add(GetStatementLine());
        }
        if (!TryPopToken(TokenType.ClosingCurlyBrace, out Token? _)) throw new Exception();
        while (TokenStack.Peek().Type == TokenType.Semicolon) TokenStack.Pop();

        // Function definition assignment
        List<Expression> definitions = [];
        var lambdas = BlockFunctionBodyStack.Pop();
        foreach (var header in headers)
        {
            definitions.Add(Expression.Assign(funcs[header], ToGMValue(lambdas[header])));
        }
        blockStmts.InsertRange(0, definitions);

        return Expression.Block(BlockScopeStack.Pop().Values.ToArray(), blockStmts);
    }

    private Expression PopAssign()
    {
        var access = PopAccess();
        var next = TokenStack.Pop();
        switch (next.Type)
        {
            case TokenType.Assignment:
            return Expression.Assign(access, PopExpression());
            case TokenType.AdditionAssignment:
            return Expression.AddAssign(access, PopExpression());
            case TokenType.SubtractionAssignment:
            return Expression.SubtractAssign(access, PopExpression());
            case TokenType.MultiplyAssignment:
            return Expression.MultiplyAssign(access, PopExpression());
            case TokenType.DivideAssignment:
            return Expression.DivideAssign(access, PopExpression());
            case TokenType.OrAssignment:
            return Expression.OrAssign(access, PopExpression());
            case TokenType.AndAssignment:
            return Expression.AndAssign(access, PopExpression());
            case TokenType.XorAssignment:
            return Expression.ExclusiveOrAssign(access, PopExpression());
            case TokenType.Increment:
            {
                ParameterExpression temp = Expression.Variable(typeof(RValue));
                return Expression.Block
                (
                    [temp],
                    Expression.Assign(temp, access),
                    Expression.AddAssign(access, Expression.Constant(new RValue(1f), typeof(RValue))),
                    temp
                );
            }
            case TokenType.Decrement:
            {
                ParameterExpression temp = Expression.Variable(typeof(RValue));
                return Expression.Block
                (
                    [temp],
                    Expression.Assign(temp, access),
                    Expression.SubtractAssign(access, Expression.Constant(new RValue(1f), typeof(RValue))),
                    temp
                );
            }
            default:
            TokenStack.Push(next);
            return access;
        }
    }

    /// <summary>
    /// This method will simply compile the next block / whole script to get function headers (which will just be function name in GML as parameters don't matters in GML).
    /// </summary>
    /// <returns></returns>
    /// <exception cref="Exception"></exception>
    private string[] GetFunctionHeaders()
    {
        Stack<Token> stack = new(TokenStack.Reverse());
        List<string> headers = [];
        
        IsFirstCompile = true;
        BlockScopeStack.Push([]);

        bool isRoot;
        isRoot = !TryPopToken(TokenType.OpeningCurlyBrace, out Token? _);
        while (TokenStack.Peek().Type != TokenType.ClosingCurlyBrace && TokenStack.Peek().Type != TokenType.Eof)
        {
            var stmt = GetStatementLine();
            if (stmt is FunctionHeaderExpression headerExpression)
            {
                headers.Add(headerExpression.Name!);
            }
        }
        if (!isRoot && !TryPopToken(TokenType.ClosingCurlyBrace, out Token? _)) throw new Exception();

        BlockScopeStack.Pop();
        IsFirstCompile = false;

        TokenStack = stack;
        return headers.ToArray();
    }

    /// <summary>
    /// Read a statement.
    /// </summary>
    /// <returns></returns>
    /// <exception cref="Exception"></exception>
    /// <remarks>
    /// <c>Increment</c> and <c>Decrement</c> expression is considered as <c>Statements</c> (prefix) or <c>Assignment Statement</c> (postfix), as GML doesn't support expression statement and it can't be supported as <c>Equality</c> token can be considered as both <c>Equality</c> and <c>Assign</c>, it's impossible to identify them.
    /// </remarks>
    private Expression GetStatement()
    {
        if (TokenStack.Peek().Type == TokenType.OpeningCurlyBrace)
        {
            return PopBlock();
        }
        else if (TokenStack.Peek().Type == TokenType.If)
        {
            TokenStack.Pop();
            var condition = Expression.NotEqual
            (
                PopExpression(),
                Expression.New(GMValueConstructor, Expression.Constant(0, typeof(object)))
            );
            if (TokenStack.Peek().Type == TokenType.Then)
            {
                TokenStack.Pop();
            }
            Expression body = GetStatementLine();
            if (TokenStack.Peek().Type == TokenType.Else)
            {
                TokenStack.Pop();
                return Expression.IfThenElse(condition, body, GetStatementLine());
            }
            return Expression.IfThen(condition, body);
        }
        else if (TokenStack.Peek().Type == TokenType.While)
        {
            TokenStack.Pop();
            var loop = new StatementLabelPair();
            var condition = Expression.NotEqual
            (
                PopExpression(),
                Expression.New(GMValueConstructor, Expression.Constant(0, typeof(object)))
            );
            StatementLabelStack.Push(loop);
            var body = GetStatementLine();
            StatementLabelStack.Pop();
            if (TokenStack.Peek().Type == TokenType.Do) TokenStack.Pop();
            return Expression.Block(Expression.Loop(Expression.Block
            (
                Expression.Label(loop.Begin),
                Expression.IfThen(Expression.Not(condition), Expression.Break(loop.End)),
                body
            )), Expression.Label(loop.End));
        }
        else if (TokenStack.Peek().Type == TokenType.Do)
        {
            TokenStack.Pop();
            var loop = new StatementLabelPair();
            StatementLabelStack.Push(loop);
            var body = GetStatementLine();
            StatementLabelStack.Pop();
            if (!TryPopToken(TokenType.Until, out Token? _)) throw new Exception("ExpectedUntil");
            var condition = Expression.NotEqual
            (
                PopExpression(),
                Expression.New(GMValueConstructor, Expression.Constant(0, typeof(object)))
            );
            return Expression.Block
            (
                Expression.Loop(Expression.Block
                (
                    Expression.Label(loop.Begin),
                    body,
                    Expression.IfThen(Expression.Not(condition), Expression.Break(loop.End))
                )), Expression.Label(loop.End)
            );
        }
        else if (TokenStack.Peek().Type == TokenType.For)
        {
            TokenStack.Pop(); // Pop "for"
            var loop = new StatementLabelPair();
            
            if (!TryPopToken(TokenType.OpeningParenthesis, out Token? _)) throw new Exception();
            
            var initializer = GetStatementLine();
            // The semicolon will be taken for the statement.
            var condition = Expression.NotEqual
            (
                PopExpression(),
                Expression.New(GMValueConstructor, Expression.Constant(0, typeof(object)))
            );
            
            if (!TryPopToken(TokenType.Semicolon, out Token? _)) throw new Exception();
            
            var iterator = GetStatementLine();
            
            if (!TryPopToken(TokenType.ClosingParenthesis, out Token? _)) throw new Exception();
            
            StatementLabelStack.Push(loop);
            var body = GetStatementLine();
            StatementLabelStack.Pop();
            
            return Expression.Block(initializer, Expression.Loop(Expression.Block
            (
                Expression.IfThen(Expression.Not(condition), Expression.Break(loop.End)),
                body,
                Expression.Label(loop.Begin),
                iterator
            )), Expression.Label(loop.End));
        }
        else if (TokenStack.Peek().Type == TokenType.Break)
        {
            TokenStack.Pop();
            return Expression.Break(StatementLabelStack.Peek().End);
        }
        else if (TokenStack.Peek().Type == TokenType.Continue)
        {
            TokenStack.Pop();
            return Expression.Continue(StatementLabelStack.Peek().Begin);
        }
        else if (TokenStack.Peek().Type == TokenType.Exit)
        {
            TokenStack.Pop();
            return Expression.Return(ReturnStack.Peek(), Expression.Constant(0));
        }
        else if (TokenStack.Peek().Type == TokenType.Return)
        {
            TokenStack.Pop();
            return Expression.Return(ReturnStack.Peek(), Expression.Field(PopExpression(), "Value"));
        }
        else if (TokenStack.Peek().Type == TokenType.Repeat)
        {
            TokenStack.Pop();
            var loop = new StatementLabelPair();
            var timesExpr = Expression.Convert(Expression.Field(PopExpression(), "Value"), typeof(float));
            StatementLabelStack.Push(loop);
            var body = GetStatementLine();
            StatementLabelStack.Pop();
            var counter = Expression.Variable(typeof(float));
            return Expression.Block
            (
                [counter],
                Expression.Assign(counter, Expression.Constant(0f)),
                Expression.Loop(Expression.Block(
                Expression.IfThen(Expression.GreaterThanOrEqual(counter, timesExpr), Expression.Break(loop.End)),
                body,
                Expression.Label(loop.Begin),
                Expression.Assign(counter, Expression.Add(counter, Expression.Constant(1f)))
        )), Expression.Label(loop.End));
        }
        else if (TokenStack.Peek().Type == TokenType.Var)
        {
            TokenStack.Pop();
            List<Expression> inits = [];
            while (TokenStack.Peek().Type == TokenType.Identifier)
            {
                var token = TokenStack.Pop();
                if (TokenStack.Peek().Type == TokenType.OpeningParenthesis)
                {
                    TokenStack.Push(token);
                    break;
                }
                var name = token.Value ?? "";
                var variable = Expression.Variable(typeof(RValue));
                BlockScopeStack.Peek().Add(name, variable);
                if (TokenStack.Peek().Type == TokenType.Assignment)
                {
                    TokenStack.Pop();
                    inits.Add(Expression.Assign(variable, PopExpression()));
                }
                //if (Context.IsBuiltIn(next.lexeme)) error(Error.BuiltinVariable);
                if (TokenStack.Peek().Type == TokenType.Comma) TokenStack.Pop();
            }
            if (inits.Count == 0) return Expression.Empty();
            return Expression.Block(inits);
        }
        else if (TokenStack.Peek().Type == TokenType.Globalvar)
        {
            TokenStack.Pop();
            List<string> strs = [];
            while (TokenStack.Peek().Type == TokenType.Identifier)
            {
                var token = TokenStack.Pop();
                if (TokenStack.Peek().Type == TokenType.OpeningParenthesis)
                {
                    TokenStack.Push(token);
                    break;
                }
                //if (Context.IsBuiltIn(next.lexeme)) error(Error.BuiltinVariable);
                strs.Add(token.Value ?? "");
                if (TokenStack.Peek().Type == TokenType.Comma) TokenStack.Pop();
            }
            foreach (var str in strs)
            {
                GlobalScope.Add(str, Expression.Variable(typeof(RValue)));
            }
            return Expression.Empty();
        }
        else if (TokenStack.Peek().Type == TokenType.With)
        {
            TokenStack.Pop();
            var expression = PopExpression();
            if (TokenStack.Peek().Type == TokenType.Do) TokenStack.Pop();
            var body = GetStatementLine();
            /*
            return new With(e, GetStatementLine(), l, c);
            */
            return Expression.Empty();
        }
        else if (TokenStack.Peek().Type == TokenType.Default)
        {
            throw new Exception("Unexpected 'default' token outside the switch statement.");
        }
        else if (TokenStack.Peek().Type == TokenType.Case)
        {
            throw new Exception("Unexpected 'case' token outside the switch statement.");
        }
        else if (TokenStack.Peek().Type == TokenType.Switch)
        {
            TokenStack.Pop();
            var expression = PopExpression();
            if (!TryPopToken(TokenType.OpeningCurlyBrace, out Token? _)) throw new Exception();
            List<Expression> exprs = [];
            List<SwitchCase> cases = [];
            var loop = new StatementLabelPair();
            StatementLabelStack.Push(loop);
            Expression defaultCase = Expression.Goto(loop.End);
            while (TokenStack.Peek().Type != TokenType.ClosingCurlyBrace && TokenStack.Peek().Type != TokenType.Eof)
            {
                if (TokenStack.Peek().Type == TokenType.Case)
                {
                    TokenStack.Pop();
                    var condition = PopExpression();
                    if (!TryPopToken(TokenType.Colon, out Token? _)) throw new Exception();
                    var label = Expression.Label();
                    cases.Add(Expression.SwitchCase(Expression.Goto(label), condition));
                    exprs.Add(Expression.Label(label));
                    while (TokenStack.Peek().Type == TokenType.Semicolon) TokenStack.Pop();
                    continue;
                }
                if (TokenStack.Peek().Type == TokenType.Default)
                {
                    TokenStack.Pop();
                    if (!TryPopToken(TokenType.Colon, out Token? _)) throw new Exception();
                    var label = Expression.Label();
                    defaultCase = Expression.Goto(label);
                    exprs.Add(Expression.Label(label));
                    while (TokenStack.Peek().Type == TokenType.Semicolon) TokenStack.Pop();
                    continue;
                }
                exprs.Add(GetStatementLine());
                while (TokenStack.Peek().Type == TokenType.Semicolon) TokenStack.Pop();
            }
            StatementLabelStack.Pop();
            if (!TryPopToken(TokenType.ClosingCurlyBrace, out Token? _)) throw new Exception();

            return Expression.Block
            (
                Expression.Switch(expression, defaultCase, [.. cases]), 
                Expression.Block(exprs), 
                Expression.Label(loop.End)
            );
        }
        else if (TokenStack.Peek().Type == TokenType.Increment)
        {
            TokenStack.Pop();
            return Expression.AddAssign(PopPrefix(), Expression.Constant(new RValue(1f), typeof(RValue)));
        }
        else if (TokenStack.Peek().Type == TokenType.Decrement)
        {
            TokenStack.Pop();
            return Expression.SubtractAssign(PopPrefix(), Expression.Constant(new RValue(1f), typeof(RValue)));
        }
        else if (TokenStack.Peek().Type == TokenType.Identifier)
        {
            return PopAssign();
        }
        else if (TokenStack.Peek().Type == TokenType.Function)
        {
            if (IsFirstCompile)
            {
                return (Expression?)PopFunctionHeader() ?? Expression.Empty();
            }
            
            var func = PopFunction(out var name, (id) => FindIdentifierExpression(id!));
            // Always be called, unless there's an isolate function expression that will never be called.
            if (name is not null) BlockFunctionBodyStack.Peek().Add(name, func);
            
            return ToGMValue(func);
        }
        else if (TokenStack.Peek().Type == TokenType.Enum)
        {
            TokenStack.Pop();
                List<Expression> initializer = [];
                var inst = Expression.Variable(typeof(RValue));
                SelfStack.Push(inst); BlockScopeStack.Push([]);
                {
                    initializer.Add(Expression.Assign(inst, ToGMValue(Expression.New(GMStructConstructor))));
                    var counter = 0;
                    while (TryPopToken(TokenType.Identifier, out var key))
                    {
                        if (TryPopToken(TokenType.Assignment, out _))
                            initializer.Add(Expression.Assign(GMMember(inst, Expression.Constant(key!.Value)), PopExpression()));
                        else
                            initializer.Add(Expression.Assign(GMMember(inst, Expression.Constant(key!.Value)), Expression.Constant(new RValue(counter))));
                        if (!TryPopToken(TokenType.Comma, out _)) break;
                        counter ++;
                    }
                    if (!TryPopToken(TokenType.ClosingCurlyBrace, out _)) throw new Exception("Expects Closing Curly Brace for struct declearation.");
                    initializer.Add(inst);
                }
                SelfStack.Pop(); BlockScopeStack.Pop();
                return Expression.Block([inst], initializer);
        }
        else
        {
            return PopAssign();
        }
    }

    /// <summary>
    /// Read a statement, then skipping all semicolon (as end token of the assignment statements, or meaningless statements).
    /// </summary>
    /// <returns></returns>
    private Expression GetStatementLine()
    {
        var statement = GetStatement();
        while (TokenStack.Peek().Type == TokenType.Semicolon) TokenStack.Pop();
        return statement;
    }
}
