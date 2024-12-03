#define FEATURE_UNDEFINED_STRUCT_MEMBER_ACCESSIBLE
#define DEBUGGER_FUNCTION_INVOKE_INFO

using GMLib;

using System.Linq.Expressions;
using System.Reflection;
using System.Diagnostics.CodeAnalysis;
using System.Dynamic;
using System.Collections;

namespace GMLSharpener;

/// <summary>
/// GameMaker Value type, it stores a C# value having the similiar effect as <c>object</c>.
/// </summary>
public struct GMValue
{
    /// <summary>
    /// 
    /// </summary>
    public static readonly float MathEpsilon = 0.00001f;

    /// <summary>
    /// The value inside the GameMaker Value.
    /// </summary>
    /// <remarks>
    /// Don't put the GameMaker Value inside the GameMaker Value.
    /// </remarks>
    public object? Value;
    
    /// <summary>
    /// Access the internal int indexed collection (usually the <c>List</c>) when box stores GameMaker Array type.
    /// </summary>
    /// <remarks>
    /// When accessing setter indexer to a non-indexed collection type, it will <b>automatically</b> trans the value inside the box into GameMaker Array, where will cause the change of variable value. This is very unsafe and the potential risk of changing struct is not very clear for me, make sure you know what you're doing.
    /// </remarks>
    /// <param name="index">The index of the element accessing.</param>
    /// <returns></returns>
    /// <exception cref="Exception"></exception>
    public GMValue this[params int[] index]
    {
        readonly get
        {
            if (!TryGetIndexes(index, out GMValue? result)) throw new Exception();
            return result ?? throw new Exception();
        }
        set
        { 
            if (!TrySetIndexes(index, value)) throw new Exception();
        }
    }
    
    /// <summary>
    /// Accessor for accessing GameMaker Array type with GameMaker Real type.
    /// The index will be rounded by force cast.
    /// </summary>
    /// <param name="index">The index of the element accessing.</param>
    /// <returns></returns>
    public GMValue this[params float[] index]
    {
        readonly get => this[index.Select(x => (int)x).ToArray()];
        set => this[index.Select(x => (int)x).ToArray()] = value;
    }
    
    /// <summary>
    /// 
    /// </summary>
    /// <param name="name"></param>
    /// <returns></returns>
    public readonly GMValue this[string name]
    {
        get => GetMember(name);
        set => SetMember(name, value);
    }

    public GMValue()
    {

    }

    public GMValue(object? value)
    {
        Value = value;
    }

    public static bool Equality(object? left, object? right)
    {
        if (left is not null && right is not null)
        {
            if (left is string strL && right is string strR)
                return strL == strR;
            if (Convert.ChangeType(left, typeof(float)) is float numL && Convert.ChangeType(right, typeof(float)) is float numR)
                return Math.Abs(numL - numR) < MathEpsilon;
            return false;
        }
        return left is null && right is null;
    }
    
#region MemberAccessor
    private readonly bool TryGetMember(string name, out GMValue result)
    {
        if (Value is IDictionary<string, object?> structure)
        {
#if FEATURE_UNDEFINED_STRUCT_MEMBER_ACCESSIBLE
            structure.TryGetValue(name, out var value);
            result = new(value);
            return true;
#else
            if (structure.TryGetValue(name, out var value))
            {       
                result = value;
                return true;
            }
#endif
        }
        if (Value is Instance instance)
        {
            if (Instance.ReservedInstanceFields.TryGetValue(name, out var member))
            {
                if (member is PropertyInfo propertyInfo)
                {
                    result = new(propertyInfo.GetValue(Value));
                    return true;
                }
                if (member is FieldInfo fieldInfo)
                {
                    result = new(fieldInfo.GetValue(Value));
                    return true;
                }
            }
            if (instance.Self is Dictionary<string, object?> self && self.TryGetValue(name, out var value))
            {
                result = new(value);
                return true;
            }
        }
        result = new(null);
        return false;
    }

    private readonly bool TrySetMember(string name, GMValue val)
    {
        if (Value is IDictionary<string, object?> gmStruct)
        {
            gmStruct[name] = val.Value;
            return true;
        }
        if (Value is Instance instance)
        {
            if (Instance.ReservedInstanceFields.TryGetValue(name, out var field))
            {
                if (field is PropertyInfo propertyInfo)
                {
                    propertyInfo.SetValue(Value, val.Value);
                    return true;
                }
                if (field is FieldInfo fieldInfo)
                {
                    fieldInfo.SetValue(Value, val.Value);
                    return true;
                }
            }
            if (instance.Self is Dictionary<string, object?> self)
            {
                self[name] = val.Value;
                return true;
            }
        }
        return false;
    }

    public readonly GMValue GetMember(string name)
    {
        if (TryGetMember(name, out GMValue value))
        {
            return value;
        }
        throw new MissingMemberException($"The member named \"{name}\" is not set before reading it.");
    }

    public readonly void SetMember(string name, GMValue val) 
    {
        if (!TrySetMember(name, val))
        {
            throw new Exception();
        }
    }
#endregion

#region Indexer
    public readonly bool TryGetIndexes(int[] indexes, out GMValue? result)
    {
        result = null;
        if (Value is List<object?> list)
        {
            List<object?> element = list;
            for (var i = 0; i < indexes.Length - 1; i++)
            {
                var index = indexes[i];
                element = (List<object?>?)element[index] ?? throw new Exception();
            }
            result = new GMValue(element[indexes.Last()]);
            return true;
        }
        return false;
    }

    public bool TrySetIndexes(int[] indexes, GMValue val)
    {
        if (indexes.Length == 0) throw new Exception();
        if (Value is not List<object?>)
        {
            Value = new List<object?>();
        }
        List<object?> element = (List<object?>)Value;
        for (var i = 0; i < indexes.Length; i++)
        {
            var index = indexes[i];
            for (;index >= element.Count;)
            {
                element.Add(null);
            }
            if (element[index] is not List<object?>)
            {
                element[index] = new List<object?>();
            }
            if (i != indexes.Length - 1)
            {
                element = (List<object?>)element[index]!;
            }
            else
            {
                element[index] = val.Value;
            }
        }
        return true;
    }
#endregion

#region Operators
    public override readonly bool Equals(object? obj)
    {
        if (obj is null)
        {
            return false;
        }
        if (obj is GMValue value)
        {
            return Equality(value.Value, Value);
        }
        return obj == Value;
    }

    public static bool operator ==(GMValue left, GMValue right)
    {
        return left.Equals(right);
    }

    public static bool operator !=(GMValue left, GMValue right)
    {
        return !(left == right);
    }

    public static bool operator <(GMValue left, GMValue right)
    {
        return (((float?)left.Value) ?? throw new Exception()) < (((float?)right.Value) ?? throw new Exception());
    }

    public static bool operator >(GMValue left, GMValue right)
    {
        return (((float?)left.Value) ?? throw new Exception()) > (((float?)right.Value) ?? throw new Exception());
    }

    public static bool operator <=(GMValue left, GMValue right)
    {
        return (((float?)left.Value) ?? throw new Exception()) <= (((float?)right.Value) ?? throw new Exception());
    }

    public static bool operator >=(GMValue left, GMValue right)
    {
        return (((float?)left.Value) ?? throw new Exception()) >= (((float?)right.Value) ?? throw new Exception());
    }

    public static GMValue operator + (GMValue value)
    {
        return new(value.Value);
    }

    public static GMValue operator - (GMValue value)
    {
        return new(-(float?)value.Value ?? throw new Exception());
    }

    public static GMValue operator +(GMValue left, GMValue right)
    {
        if (left.Value is string str)
        {
            return new(str + right.Value?.ToString() ?? throw new Exception());
        }
        return new((float?)left.Value + (float?)right.Value);
    }

    public static GMValue operator -(GMValue left, GMValue right)
    {
        return new((float?)left.Value - (float?)right.Value);
    }

    public static GMValue operator %(GMValue left, GMValue right)
    {
        return new((float?)left.Value % (float?)right.Value);
    }

    public static GMValue operator *(GMValue left, GMValue right)
    {
        if (left.Value is string str)
        {
            return new(string.Concat(Enumerable.Repeat(str, (int?)(float?)right.Value ?? throw new Exception())));
        }
        return new((float?)left.Value * (float?)right.Value);
    }

    public static GMValue operator /(GMValue left, GMValue right)
    {
        return new((float?)left.Value / (float?)right.Value);
    }
#endregion

    public override readonly string ToString()
    {
        // This prefix won't shown if you use show_debug_message to dump,
        // since that will capture the Value field only.
        return "GMValue: " + Value?.ToString() ?? "Null";
    }

    public override readonly int GetHashCode()
    {
        return Value?.GetHashCode() ?? throw new Exception();
    }
}

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
    internal static readonly MethodInfo Print = typeof(GM).GetMethod("show_debug_message") ?? throw new Exception("Debugger doesn't exists"); // Debugger, can be inserted into the Expression to print certain value.

    /// <summary>
    /// Invoke C# Method of GameMaker Callable with value boxed in GameMaker Value struct.
    /// GameMaker Value I/O repacked. 
    /// This method is <b>not</b> supposed to be called by user, the only reason it's public is because otherwise it will be discarded by compiler.
    /// </summary>
    /// <param name="callable">     The callable method which must be either C# MethodInfo or GameMaker Method/Script/Function</param>
    /// <param name="arguments">    The argument array of GameMaker boxed value</param>
    /// <returns>The return value of target method, null if no return value exists</returns>
    /// <exception cref="Exception">The type of the callable is nether MethodInfo or Method</exception>
    public static GMValue InvokeWithGMValue(object caller, object callable, params GMValue[] arguments)
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
        else if (callable is Script script)
        {
            self = new Method(caller, script) { Name = script.Name };
            method = GMMethodInvoker;
        }
        else if (callable is Script.Function function)
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
            return new GMValue(method.Invoke(self, [null]));
        }
        else return new GMValue(method.Invoke(self, [.. args]));
    }

#region Expression Tree Reflections
    // Methods
    private static readonly MethodInfo GMInvoker = typeof(ExpressionParser).GetMethod("InvokeWithGMValue") ?? throw new Exception("GM Value Invoker doesn't exists");
    private static readonly MethodInfo GMMethodInvoker = typeof(Method).GetMethod("Invoke") ?? throw new Exception("Method Invoker doesn't exists");
    private static readonly MethodInfo GMScriptGetter = typeof(ExpressionParser).GetMethod("ResolveScriptHandle") ?? throw new Exception("Script Getter doesn't exists");

    // Ctors
    private static readonly ConstructorInfo GMValueConstructor = typeof(GMValue).GetConstructor([typeof(object)]) ?? throw new Exception();
    private static readonly ConstructorInfo GMMethodConstructor = typeof(Method).GetConstructor([typeof(object), typeof(Script)]) ?? throw new Exception();
    private static readonly ConstructorInfo GMStructConstructor = typeof(ExpandoObject).GetConstructor(Type.EmptyTypes) ?? throw new Exception("");

    /// Accessors
    private static readonly PropertyInfo GMValueIndexer = typeof(GMValue).GetProperties().First(x => x.GetIndexParameters().Select(y => y.ParameterType).SequenceEqual([typeof(float[])])) ?? throw new Exception("RValue missing indexer.");
    private static readonly PropertyInfo GMValueMemberAccessor = typeof(GMValue).GetProperties().First(x => x.GetIndexParameters().Select(y => y.ParameterType).SequenceEqual([typeof(string)])) ?? throw new Exception("RValue missing member accessor.");
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
        return Expression.Call(null, GMInvoker, [Expression.Convert(self, typeof(object)), Expression.Convert(function, typeof(object)), Expression.NewArrayInit(typeof(GMValue), arguments)]);
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
        if (original.Type == typeof(GMValue))
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
    public static Script ResolveScriptHandle(Handle<Script> handle)
    {
        if (!GM.FunctionManager.TryFindAsset(handle, out var asset)) throw new Exception();
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
        if (typeof(GM).GetMethod(name) is MethodInfo runtimeFunction)
        {
            return Expression.Constant(new GMValue() { Value = runtimeFunction });
        }
        if (GM.FunctionManager.TryFindAssetHandle(name, out var handle) && GM.FunctionManager.TryFindAsset(handle, out var script))
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
    private Stack<Dictionary<string, Expression<Script.Function>>> BlockFunctionBodyStack; // Stores the function definations and push them to the top of the block / script to implement the function raise
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
        Token token;
        do
        {
            token = lexer.Scan();
            TokenStack.Push(token);
        }
        while (token != Tokens.Eof);
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
        while (TokenStack.Peek().Type != TokenKind.Eof)
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
        while (TokenStack.Peek().Type != TokenKind.Eof)
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
            var script = new Script(static (_, _) => {}, header);
            GM.FunctionManager.Register(script);
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

        // Catch function definations
        BlockFunctionBodyStack.Push([]);
        while (TokenStack.Peek().Type != TokenKind.Eof)
        {
            GetStatementLine();
        }
        var lambdas = BlockFunctionBodyStack.Pop();
        
        // Register functions
        foreach (var header in headers)
        {
            var script = new Script(lambdas[header].Compile(), header);
            GM.FunctionManager.Override(script);
        }                
    }
}

// Main Parser
public partial class ExpressionParser
{
    private bool TryPopToken(TokenKind type, out Token? token)
    {
        if (TokenStack.Peek().Type == type)
        {
            token = TokenStack.Pop();
            return true;
        }
        token = null;
        return false;
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
                case TokenKind.LogicalAnd:
                TokenStack.Pop();
                val = Expression.And(val, PopRelational());
                continue;
                
                case TokenKind.LogicalOr:
                TokenStack.Pop();
                val = Expression.Or(val, PopRelational());
                
                continue;
                case TokenKind.LogicalXor:
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
                case TokenKind.LessThan:
                TokenStack.Pop();
                val = ToGMValue(Expression.LessThan(val, PopBitwise()));
                continue;

                case TokenKind.LessThanOrEqual:
                TokenStack.Pop();
                val = ToGMValue(Expression.LessThanOrEqual(val, PopBitwise()));
                continue;

                case TokenKind.GreaterThan:
                TokenStack.Pop();
                val = ToGMValue(Expression.GreaterThan(val, PopBitwise()));
                continue;

                case TokenKind.GreaterThanOrEqual:
                TokenStack.Pop();
                val = ToGMValue(Expression.GreaterThanOrEqual(val, PopBitwise()));
                continue;
                case TokenKind.Inequality:

                TokenStack.Pop();
                val = ToGMValue(Expression.NotEqual(val, PopBitwise()));
                continue;

                case TokenKind.Equality:
                case TokenKind.Assignment:
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
                case TokenKind.BitwiseAnd:
                TokenStack.Pop();
                val = Expression.And(val, PopRelational());
                continue;
                
                case TokenKind.BitwiseOr:
                TokenStack.Pop();
                val = Expression.Or(val, PopRelational());
                
                continue;
                case TokenKind.BitwiseXor:
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
                case TokenKind.ShiftLeft:
                TokenStack.Pop();
                val = Expression.LeftShift(val, PopAdditive());
                continue;
                
                case TokenKind.ShiftRight:
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
                case TokenKind.Plus:
                TokenStack.Pop();
                val = Expression.Add(val, PopTerm());
                continue;
                
                case TokenKind.Minus:
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
                case TokenKind.Multiply:
                TokenStack.Pop();
                val = Expression.Multiply(val, PopPrefix());
                continue;

                case TokenKind.Divide:
                TokenStack.Pop();
                val = Expression.Divide(val, PopPrefix());
                continue;

                case TokenKind.Mod:
                TokenStack.Pop();
                val = Expression.Modulo(val, PopPrefix());
                continue;

                case TokenKind.Div:
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
            case TokenKind.Plus:
            TokenStack.Pop();
            return PopPrefix();

            case TokenKind.Minus:
            TokenStack.Pop();
            return Expression.Negate(PopPrefix());

            case TokenKind.Not:
            TokenStack.Pop();
            return Expression.Not(PopPrefix());
            
            case TokenKind.BitwiseComplement:
            TokenStack.Pop();
            return Expression.Not(PopPrefix());
            
            case TokenKind.Increment:
            TokenStack.Pop();
            return Expression.AddAssign(PopPrefix(), Expression.Constant(new GMValue(1f), typeof(GMValue)));

            case TokenKind.Decrement:
            TokenStack.Pop();
            return Expression.SubtractAssign(PopPrefix(), Expression.Constant(new GMValue(1f), typeof(GMValue)));

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
                case TokenKind.Increment:
                {
                    TokenStack.Pop();
                    ParameterExpression temp = Expression.Variable(typeof(GMValue));
                    val = Expression.Block
                    (
                        [temp],
                        Expression.Assign(temp, val),
                        Expression.AddAssign(val, Expression.Constant(new GMValue(1f), typeof(GMValue))),
                        temp
                    );
                }
                break;
                
                case TokenKind.Decrement:
                {
                    TokenStack.Pop();
                    ParameterExpression temp = Expression.Variable(typeof(GMValue));
                    val = Expression.Block
                    (
                        [temp],
                        Expression.Assign(temp, val),
                        Expression.SubtractAssign(val, Expression.Constant(new GMValue(1f), typeof(GMValue))),
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
        bool isNew = TryPopToken(TokenKind.New, out _); // If new is detected, capture the first method caller as ctor
        bool isCtorCaptured = false;

        // The call and access is combined as they can appear together as A = A() | A.B | I[B]
        Expression accessed = Expression.New(GMValueConstructor, Expression.Constant(null, typeof(object)));
        Expression val = PopFactor();

        while (true)
        {
            switch (TokenStack.Peek().Type)
            {
                case TokenKind.Dot:
                accessed = val;
                TokenStack.Pop();
                if (!TryPopToken(TokenKind.Identifier, out Token? member))
                {
                    throw new Exception($"Expected Member name, got {member?.Type}.");
                }
                val = GMMember(val, Expression.Constant(member!.Lexeme!));
                break;
                
                case TokenKind.OpeningSquareBracket:
                val = GMIndexer(val, PopIndices());
                break;

                case TokenKind.OpeningParenthesis:
                var args = PopArguments();
                if (isNew && !isCtorCaptured)
                {
                    var caller = Expression.Variable(typeof(GMValue));
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
        if (!TryPopToken(TokenKind.OpeningParenthesis, out _)) throw new Exception("Expected closing parenthesis for callable invokement.");
        while (TokenStack.Peek().Type == TokenKind.Identifier)
        {
            var token = TokenStack.Pop();
            var name = token.Lexeme ?? "";
            var variable = Expression.Variable(typeof(GMValue), name);
            BlockScopeStack.Peek().Add(name, variable);
            if (TokenStack.Peek().Type == TokenKind.Assignment)
            {
                TokenStack.Pop();
                inits.Add(Expression.Assign(variable, PopExpression()));
            }
            if (TokenStack.Peek().Type == TokenKind.Comma) TokenStack.Pop();
        }
        if (!TryPopToken(TokenKind.ClosingParenthesis, out _)) throw new Exception("Expected closing parenthesis for callable invokement.");
        if (inits.Count == 0) initializer = Expression.Empty();
        else initializer = Expression.Block(inits);
    }

    private Expression[] PopArguments()
    {
        if (!TryPopToken(TokenKind.OpeningParenthesis, out _))
        {
            throw new Exception("Expected closing parenthesis for callable invokement.");
        }
        List<Expression> arguments = [];
        var next = TokenStack.Peek();
        if (next.Type != TokenKind.ClosingParenthesis && next.Type != TokenKind.Eof)
        {
            arguments.Add(PopExpression());
        }
        while (TokenStack.Peek().Type == TokenKind.Comma)
        {
            TokenStack.Pop();
            arguments.Add(PopExpression());
        }
        if (!TryPopToken(TokenKind.ClosingParenthesis, out _))
        {
            throw new Exception("Expected closing parenthesis for callable invokement.");
        }
        return [..arguments];
    }

    private Expression[] PopIndices()
    {
        if (TokenStack.Peek().Type != TokenKind.OpeningSquareBracket)
            throw new Exception($"Expected Closing Square Bracket, got {TokenStack.Peek().Type}");
        TokenStack.Pop();

        List<Expression> indices = [];
        while (TokenStack.Peek().Type != TokenKind.ClosingSquareBracket)
        {
            indices.Add(PopExpression());
            var next = TokenStack.Peek();
            if (next.Type == TokenKind.Comma)
                TokenStack.Pop();
            else if (next.Type != TokenKind.ClosingSquareBracket)
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
        TryPopToken(TokenKind.Identifier, out var tokenName); string? name = tokenName?.Lexeme;
        // Try pop arguments
        BlockScopeStack.Push([]); PopParameters(out _); BlockScopeStack.Pop();
        // Try pop base constructor
        if (TryPopToken(TokenKind.Colon, out _)) PopAccess(); TryPopToken(TokenKind.Constructor, out _);
        // Pop function body.
        PopBlock();
        if (name is not null) return new FunctionHeaderExpression(name);
        return null;
    }

    private Expression<Script.Function> PopFunction(out string? name, Func<string?, ParameterExpression?> calleeParser)
    {
        TokenStack.Pop();
        // Try pop the name for the function.
        name = null;
        if (TryPopToken(TokenKind.Identifier, out var tokenName)) name = tokenName!.Lexeme ?? throw new Exception();
        
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
            if (TryPopToken(TokenKind.Colon, out _))
            {
                baseCtor = GMCall(ToGMValue(caller), PopFactor(), PopArguments());
                isCtor = true;
            }

            // Check if the function is constructor.
            if (TryPopToken(TokenKind.Constructor, out _))
            {
                isCtor = true;
            } // The exception only occur if having base Ctors while doesn't have constructor keyword.
            else if (isCtor) throw new Exception("Expects Constructor keywords since base ctor is defined.");

            // Pop function body.
            BlockScopeStack.Push(name is not null ? new() {{ name, calleeParser(name) ?? throw new Exception("") }} : []);

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

        return Expression.Lambda<Script.Function>(body, name, [caller, arguments]);
    }
#endregion

    private Expression PopFactor()
    {
        Expression expression;
        switch (TokenStack.Peek().Type)
        {
            /// Number
            case TokenKind.Real:
            return Expression.Constant(new GMValue() { Value = ((RealToken)TokenStack.Pop()).Value }, typeof(GMValue));
            /// String
            case TokenKind.StringLiteral:
            return Expression.Constant(new GMValue() { Value = ((StringLiteralToken)TokenStack.Pop()).Value }, typeof(GMValue));
            /// Boolean
            case TokenKind.Boolean:
            return Expression.Constant(new GMValue() { Value = ((BooleanToken)TokenStack.Pop()).Value }, typeof(GMValue));
            /// Asset / Field / Variable
            case TokenKind.Identifier:
            // In expection, this case will only be called for the first identifier
            // I'm not sure if it will be called out of such condition
            return FindIdentifierExpression(TokenStack.Pop().Lexeme!) ?? throw new Exception("Unclaimed variable");
            /// Group
            case TokenKind.OpeningParenthesis:
            {
                TokenStack.Pop();
                expression = Expression.Block(PopExpression());
                if (!TryPopToken(TokenKind.ClosingParenthesis, out Token? _))
                    throw new Exception("Expected Closing Parenthesis.");
                return expression;
            }
            /// Struct
            case TokenKind.OpeningCurlyBrace:
            {
                TokenStack.Pop();
                List<Expression> initializer = [];
                var inst = Expression.Variable(typeof(GMValue));
                SelfStack.Push(inst); BlockScopeStack.Push([]);
                {
                    initializer.Add(Expression.Assign(inst, ToGMValue(Expression.New(GMStructConstructor))));
                    while (TryPopToken(TokenKind.Identifier, out var key))
                    {
                        if (TryPopToken(TokenKind.Colon, out _))
                        {
                            initializer.Add(Expression.Assign(GMMember(inst, Expression.Constant(key!.Lexeme)), PopExpression()));
                        }
                        else throw new Exception("Expects value assignment for struct.");
                        if (!TryPopToken(TokenKind.Comma, out _)) break;
                    }
                    if (!TryPopToken(TokenKind.ClosingCurlyBrace, out _)) throw new Exception("Expects Closing Curly Brace for struct declearation.");
                    initializer.Add(inst);
                }
                SelfStack.Pop(); BlockScopeStack.Pop();
                return Expression.Block([inst], initializer);
            }
            /// Function
            case TokenKind.Function:
            {  
                var callee  = Expression.Parameter(typeof(GMValue), "LAMBDA_CALLEE");
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
            var counter = 1;
            if (!TryPopToken(TokenKind.OpeningCurlyBrace, out Token? _)) throw new Exception();
            while (counter > 0)
            {
                var token = TokenStack.Pop();
                if (token.Type == TokenKind.OpeningCurlyBrace) counter ++;
                if (token.Type == TokenKind.ClosingCurlyBrace) counter --;
            }
            return Expression.Empty();
        }

        // Capture function headers
        Dictionary<string, ParameterExpression> funcs = [];
        var headers = GetFunctionHeaders();
        foreach (var header in headers)
        {
            funcs.Add(header, Expression.Parameter(typeof(GMValue)));
        }

        // Push block scope
        BlockScopeStack.Push(funcs); BlockFunctionBodyStack.Push([]);
        // Capture body statements
        List<Expression> blockStmts = [];
        if (!TryPopToken(TokenKind.OpeningCurlyBrace, out Token? _)) throw new Exception();
        while (TokenStack.Peek().Type != TokenKind.ClosingCurlyBrace && TokenStack.Peek().Type != TokenKind.Eof)
        {
            blockStmts.Add(GetStatementLine());
        }
        if (!TryPopToken(TokenKind.ClosingCurlyBrace, out Token? _)) throw new Exception();
        while (TokenStack.Peek().Type == TokenKind.Semicolon) TokenStack.Pop();

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
            case TokenKind.Assignment:
            return Expression.Assign(access, PopExpression());
            case TokenKind.AdditionAssignment:
            return Expression.AddAssign(access, PopExpression());
            case TokenKind.SubtractionAssignment:
            return Expression.SubtractAssign(access, PopExpression());
            case TokenKind.MultiplyAssignment:
            return Expression.MultiplyAssign(access, PopExpression());
            case TokenKind.DivideAssignment:
            return Expression.DivideAssign(access, PopExpression());
            case TokenKind.OrAssignment:
            return Expression.OrAssign(access, PopExpression());
            case TokenKind.AndAssignment:
            return Expression.AndAssign(access, PopExpression());
            case TokenKind.XorAssignment:
            return Expression.ExclusiveOrAssign(access, PopExpression());
            case TokenKind.Increment:
            {
                ParameterExpression temp = Expression.Variable(typeof(GMValue));
                return Expression.Block
                (
                    [temp],
                    Expression.Assign(temp, access),
                    Expression.AddAssign(access, Expression.Constant(new GMValue(1f), typeof(GMValue))),
                    temp
                );
            }
            case TokenKind.Decrement:
            {
                ParameterExpression temp = Expression.Variable(typeof(GMValue));
                return Expression.Block
                (
                    [temp],
                    Expression.Assign(temp, access),
                    Expression.SubtractAssign(access, Expression.Constant(new GMValue(1f), typeof(GMValue))),
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
        isRoot = !TryPopToken(TokenKind.OpeningCurlyBrace, out Token? _);
        while (TokenStack.Peek().Type != TokenKind.ClosingCurlyBrace && TokenStack.Peek().Type != TokenKind.Eof)
        {
            var stmt = GetStatementLine();
            if (stmt is FunctionHeaderExpression headerExpression)
            {
                headers.Add(headerExpression.Name!);
            }
        }
        if (!isRoot && !TryPopToken(TokenKind.ClosingCurlyBrace, out Token? _)) throw new Exception();

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
        if (TokenStack.Peek().Type == TokenKind.OpeningCurlyBrace)
        {
            return PopBlock();
        }
        else if (TokenStack.Peek().Type == TokenKind.If)
        {
            TokenStack.Pop();
            var condition = Expression.NotEqual
            (
                PopExpression(),
                Expression.New(GMValueConstructor, Expression.Constant(0, typeof(object)))
            );
            if (TokenStack.Peek().Type == TokenKind.Then)
            {
                TokenStack.Pop();
            }
            Expression body = GetStatementLine();
            if (TokenStack.Peek().Type == TokenKind.Else)
            {
                TokenStack.Pop();
                return Expression.IfThenElse(condition, body, GetStatementLine());
            }
            return Expression.IfThen(condition, body);
        }
        else if (TokenStack.Peek().Type == TokenKind.While)
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
            if (TokenStack.Peek().Type == TokenKind.Do) TokenStack.Pop();
            return Expression.Block(Expression.Loop(Expression.Block
            (
                Expression.Label(loop.Begin),
                Expression.IfThen(Expression.Not(condition), Expression.Break(loop.End)),
                body
            )), Expression.Label(loop.End));
        }
        else if (TokenStack.Peek().Type == TokenKind.Do)
        {
            TokenStack.Pop();
            var loop = new StatementLabelPair();
            StatementLabelStack.Push(loop);
            var body = GetStatementLine();
            StatementLabelStack.Pop();
            if (!TryPopToken(TokenKind.Until, out Token? _)) throw new Exception("ExpectedUntil");
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
        else if (TokenStack.Peek().Type == TokenKind.For)
        {
            TokenStack.Pop();
            var loop = new StatementLabelPair();
            // "For" is very weird in GM. Any legal statement including control flow and blocks can be used in for.
            // example of legal "for" statements:
            // for (i = 0 i<3; {case 3:exit};;;)func();
            if (!TryPopToken(TokenKind.OpeningParenthesis, out Token? _)) throw new Exception();
            var initializer = GetStatementLine();
            //match(Token.Semicolon); // Taken care of by stmt();
            var condition = Expression.NotEqual
            (
                PopExpression(),
                Expression.New(GMValueConstructor, Expression.Constant(0, typeof(object)))
            );
            if (!TryPopToken(TokenKind.Semicolon, out Token? _)) throw new Exception();
            var iterator = GetStatementLine();
            if (!TryPopToken(TokenKind.ClosingParenthesis, out Token? _)) throw new Exception();
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
        else if (TokenStack.Peek().Type == TokenKind.Break)
        {
            TokenStack.Pop();
            return Expression.Break(StatementLabelStack.Peek().End);
        }
        else if (TokenStack.Peek().Type == TokenKind.Continue)
        {
            TokenStack.Pop();
            return Expression.Continue(StatementLabelStack.Peek().Begin);
        }
        else if (TokenStack.Peek().Type == TokenKind.Exit)
        {
            TokenStack.Pop();
            return Expression.Return(ReturnStack.Peek(), Expression.Constant(0));
        }
        else if (TokenStack.Peek().Type == TokenKind.Return)
        {
            TokenStack.Pop();
            return Expression.Return(ReturnStack.Peek(), Expression.Field(PopExpression(), "Value"));
        }
        else if (TokenStack.Peek().Type == TokenKind.Repeat)
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
        else if (TokenStack.Peek().Type == TokenKind.Var)
        {
            TokenStack.Pop();
            List<Expression> inits = [];
            while (TokenStack.Peek().Type == TokenKind.Identifier)
            {
                var token = TokenStack.Pop();
                if (TokenStack.Peek().Type == TokenKind.OpeningParenthesis)
                {
                    TokenStack.Push(token);
                    break;
                }
                var name = token.Lexeme ?? "";
                var variable = Expression.Variable(typeof(GMValue));
                BlockScopeStack.Peek().Add(name, variable);
                if (TokenStack.Peek().Type == TokenKind.Assignment)
                {
                    TokenStack.Pop();
                    inits.Add(Expression.Assign(variable, PopExpression()));
                }
                //if (Context.IsBuiltIn(next.lexeme)) error(Error.BuiltinVariable);
                if (TokenStack.Peek().Type == TokenKind.Comma) TokenStack.Pop();
            }
            if (inits.Count == 0) return Expression.Empty();
            return Expression.Block(inits);
        }
        else if (TokenStack.Peek().Type == TokenKind.Globalvar)
        {
            TokenStack.Pop();
            List<string> strs = [];
            while (TokenStack.Peek().Type == TokenKind.Identifier)
            {
                var token = TokenStack.Pop();
                if (TokenStack.Peek().Type == TokenKind.OpeningParenthesis)
                {
                    TokenStack.Push(token);
                    break;
                }
                //if (Context.IsBuiltIn(next.lexeme)) error(Error.BuiltinVariable);
                strs.Add(token.Lexeme ?? "");
                if (TokenStack.Peek().Type == TokenKind.Comma) TokenStack.Pop();
            }
            foreach (var str in strs)
            {
                GlobalScope.Add(str, Expression.Variable(typeof(GMValue)));
            }
            return Expression.Empty();
        }
        else if (TokenStack.Peek().Type == TokenKind.With)
        {
            TokenStack.Pop();
            var expression = PopExpression();
            if (TokenStack.Peek().Type == TokenKind.Do) TokenStack.Pop();
            var body = GetStatementLine();
            /*
            return new With(e, GetStatementLine(), l, c);
            */
            return Expression.Empty();
        }
        else if (TokenStack.Peek().Type == TokenKind.Default)
        {
            throw new Exception("Unexpected 'default' token outside the switch statement.");
        }
        else if (TokenStack.Peek().Type == TokenKind.Case)
        {
            throw new Exception("Unexpected 'case' token outside the switch statement.");
        }
        else if (TokenStack.Peek().Type == TokenKind.Switch)
        {
            TokenStack.Pop();
            var expression = PopExpression();
            if (!TryPopToken(TokenKind.OpeningCurlyBrace, out Token? _)) throw new Exception();
            List<Expression> exprs = [];
            List<SwitchCase> cases = [];
            var loop = new StatementLabelPair();
            StatementLabelStack.Push(loop);
            Expression defaultCase = Expression.Goto(loop.End);
            while (TokenStack.Peek().Type != TokenKind.ClosingCurlyBrace && TokenStack.Peek().Type != TokenKind.Eof)
            {
                if (TokenStack.Peek().Type == TokenKind.Case)
                {
                    TokenStack.Pop();
                    var condition = PopExpression();
                    if (!TryPopToken(TokenKind.Colon, out Token? _)) throw new Exception();
                    var label = Expression.Label();
                    cases.Add(Expression.SwitchCase(Expression.Goto(label), condition));
                    exprs.Add(Expression.Label(label));
                    while (TokenStack.Peek().Type == TokenKind.Semicolon) TokenStack.Pop();
                    continue;
                }
                if (TokenStack.Peek().Type == TokenKind.Default)
                {
                    TokenStack.Pop();
                    if (!TryPopToken(TokenKind.Colon, out Token? _)) throw new Exception();
                    var label = Expression.Label();
                    defaultCase = Expression.Goto(label);
                    exprs.Add(Expression.Label(label));
                    while (TokenStack.Peek().Type == TokenKind.Semicolon) TokenStack.Pop();
                    continue;
                }
                exprs.Add(GetStatementLine());
                while (TokenStack.Peek().Type == TokenKind.Semicolon) TokenStack.Pop();
            }
            StatementLabelStack.Pop();
            if (!TryPopToken(TokenKind.ClosingCurlyBrace, out Token? _)) throw new Exception();

            return Expression.Block
            (
                Expression.Switch(expression, defaultCase, [.. cases]), 
                Expression.Block(exprs), 
                Expression.Label(loop.End)
            );
        }
        else if (TokenStack.Peek().Type == TokenKind.Increment)
        {
            TokenStack.Pop();
            return Expression.AddAssign(PopPrefix(), Expression.Constant(new GMValue(1f), typeof(GMValue)));
        }
        else if (TokenStack.Peek().Type == TokenKind.Decrement)
        {
            TokenStack.Pop();
            return Expression.SubtractAssign(PopPrefix(), Expression.Constant(new GMValue(1f), typeof(GMValue)));
        }
        else if (TokenStack.Peek().Type == TokenKind.Identifier)
        {
            return PopAssign();
        }
        else if (TokenStack.Peek().Type == TokenKind.Function)
        {
            if (IsFirstCompile)
            {
                return (Expression?)PopFunctionHeader() ?? Expression.Empty();
            }
            
            var func = PopFunction(out var name, (id) => (ParameterExpression?)FindIdentifierExpression(id!));
            // Always be called, unless there's an isolate function expression that will never be called.
            if (name is not null) BlockFunctionBodyStack.Peek().Add(name, func);
            
            return ToGMValue(func);
        }
        else if (TokenStack.Peek().Type == TokenKind.Enum)
        {
            TokenStack.Pop();
                List<Expression> initializer = [];
                var inst = Expression.Variable(typeof(GMValue));
                SelfStack.Push(inst); BlockScopeStack.Push([]);
                {
                    initializer.Add(Expression.Assign(inst, ToGMValue(Expression.New(GMStructConstructor))));
                    var counter = 0;
                    while (TryPopToken(TokenKind.Identifier, out var key))
                    {
                        if (TryPopToken(TokenKind.Assignment, out _))
                            initializer.Add(Expression.Assign(GMMember(inst, Expression.Constant(key!.Lexeme)), PopExpression()));
                        else
                            initializer.Add(Expression.Assign(GMMember(inst, Expression.Constant(key!.Lexeme)), Expression.Constant(new GMValue(counter))));
                        if (!TryPopToken(TokenKind.Comma, out _)) break;
                        counter ++;
                    }
                    if (!TryPopToken(TokenKind.ClosingCurlyBrace, out _)) throw new Exception("Expects Closing Curly Brace for struct declearation.");
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
        while (TokenStack.Peek().Type == TokenKind.Semicolon) TokenStack.Pop();
        return statement;
    }
}
