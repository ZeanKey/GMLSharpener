using GMLib;
using System.Linq.Expressions;
using System.Reflection;
using System.Diagnostics.CodeAnalysis;
using System.Dynamic;
using System.Collections;

namespace GMLSharpener.GM;

/// <summary>
/// GameMaker Value type, it stores a C# value having the similiar effect as <c>object</c>.
/// </summary>
public struct RValue
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
    public RValue this[params int[] index]
    {
        readonly get
        {
            if (!TryGetIndexes(index, out RValue? result)) throw new Exception();
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
    public RValue this[params float[] index]
    {
        readonly get => this[index.Select(x => (int)x).ToArray()];
        set => this[index.Select(x => (int)x).ToArray()] = value;
    }
    
    /// <summary>
    /// 
    /// </summary>
    /// <param name="name"></param>
    /// <returns></returns>
    public readonly RValue this[string name]
    {
        get => GetMember(name);
        set => SetMember(name, value);
    }

    public RValue()
    {

    }

    public RValue(object? value)
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
    private readonly bool TryGetMember(string name, out RValue result)
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
                result = new(value);
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

    private readonly bool TrySetMember(string name, RValue val)
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

    public readonly RValue GetMember(string name)
    {
        if (TryGetMember(name, out RValue value))
        {
            return value;
        }
        throw new MissingMemberException($"The member named \"{name}\" is not set before reading it.");
    }

    public readonly void SetMember(string name, RValue val) 
    {
        if (!TrySetMember(name, val))
        {
            throw new Exception();
        }
    }
#endregion

#region Indexer
    public readonly bool TryGetIndexes(int[] indexes, out RValue? result)
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
            result = new RValue(element[indexes.Last()]);
            return true;
        }
        return false;
    }

    public bool TrySetIndexes(int[] indexes, RValue val)
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
        if (obj is RValue value)
        {
            return Equality(value.Value, Value);
        }
        return obj == Value;
    }

    public static bool operator ==(RValue left, RValue right)
    {
        return left.Equals(right);
    }

    public static bool operator !=(RValue left, RValue right)
    {
        return !(left == right);
    }

    public static bool operator <(RValue left, RValue right)
    {
        return (((float?)left.Value) ?? throw new Exception()) < (((float?)right.Value) ?? throw new Exception());
    }

    public static bool operator >(RValue left, RValue right)
    {
        return (((float?)left.Value) ?? throw new Exception()) > (((float?)right.Value) ?? throw new Exception());
    }

    public static bool operator <=(RValue left, RValue right)
    {
        return (((float?)left.Value) ?? throw new Exception()) <= (((float?)right.Value) ?? throw new Exception());
    }

    public static bool operator >=(RValue left, RValue right)
    {
        return (((float?)left.Value) ?? throw new Exception()) >= (((float?)right.Value) ?? throw new Exception());
    }

    public static RValue operator + (RValue value)
    {
        return new(value.Value);
    }

    public static RValue operator - (RValue value)
    {
        return new(-(float?)value.Value ?? throw new Exception());
    }

    public static RValue operator +(RValue left, RValue right)
    {
        if (left.Value is string str)
        {
            return new(str + right.Value?.ToString() ?? throw new Exception());
        }
        return new((float?)left.Value + (float?)right.Value);
    }

    public static RValue operator -(RValue left, RValue right)
    {
        return new((float?)left.Value - (float?)right.Value);
    }

    public static RValue operator %(RValue left, RValue right)
    {
        return new((float?)left.Value % (float?)right.Value);
    }

    public static RValue operator *(RValue left, RValue right)
    {
        if (left.Value is string str)
        {
            return new(string.Concat(Enumerable.Repeat(str, (int?)(float?)right.Value ?? throw new Exception())));
        }
        return new((float?)left.Value * (float?)right.Value);
    }

    public static RValue operator /(RValue left, RValue right)
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
