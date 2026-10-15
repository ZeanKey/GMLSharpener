# GMLSharpener

**简体中文** | [English](README.md)

GMLSharpener（GameMaker Language Sharpener）是一个基于 C# 和 .NET 8 的 GML 编译工具库，提供在运行时读取、解析和执行项目中 GameMaker Language（GMLv2）代码明文的能力。它将源码解析为 LINQ 表达式树，再通过 .NET 的表达式编译机制生成可执行代码。

GMLSharpener 可将代码明文导出为独立的可调用函数，并支持导出该函数内部定义的嵌套函数与方法。

项目依赖独立的运行时库 **GMLib**，提供实例、脚本资源和原生函数等运行支持。房间等依赖游戏生命周期的功能，仅在由 GMLib 驱动的 raylib-cs 游戏主程序中生效；调用这些功能需要相应的游戏运行环境。

> 项目仍在开发中，尚未完整实现 GMLv2。已有功能也需要进一步验证，不应视为与 GameMaker 完全兼容。

## 工作方式

```text
GML 源码
   ↓ Lexer：词法分析
Token 序列
   ↓ ExpressionParser：语法解析
LINQ 表达式树
   ↓ Compile()
可执行委托（依赖 GMLib 运行）
```

`RValue` 用于包装运行时值，并提供运算、索引和成员访问等操作。

## 开发环境与依赖

- .NET 8 SDK。
- 支持 C#/.NET 的开发工具，例如 JetBrains Rider。
- GMLib 源码及其依赖。

当前通过 `ProjectReference` 引用相邻目录中的 GMLib：

```text
工作目录/
├── GMLib/
│   └── GMLib.csproj
└── GMLSharpener/
    ├── GMLSharpener.sln
    └── GMLSharpener.csproj
```

GMLib 需要单独获取；仅克隆本仓库不足以完成构建。项目目前未在本仓库中固定兼容的 GMLib 版本，因此开发时还需确认两者的 API 匹配。

## 构建

准备好上述目录后，在 GMLSharpener 根目录执行：

```sh
dotnet restore GMLSharpener.csproj
dotnet build GMLSharpener.csproj --no-restore
```

也可以在 Rider 中打开 `GMLSharpener.sln`，恢复依赖后构建。需要同时阅读和调试 GMLib 时，可以将其作为已有项目加入 Solution；两个库仍可分别由各自的 Git 仓库管理。

若提示找不到 GMLib 的项目信息，先确认 `../GMLib/GMLib.csproj` 存在，再检查依赖恢复结果。若调整源码目录，需要同步修改项目引用路径。

## 主要入口

创建 `ExpressionParser` 时传入 `IDictionary<string, object?>`，作为解析器绑定的全局上下文。

| API | 用途 |
| --- | --- |
| `ParseScript` | 将脚本解析为表达式树，返回的 Lambda 可编译为无参数委托 |
| `ParseObjectEvent` | 解析对象事件，生成接收实例上下文的事件 Lambda |
| `ParseExportScriptPreCompile` | 预先注册导出脚本中的函数名称 |
| `ParseExportScript` | 编译导出函数，并更新已注册的脚本资源 |

导出脚本采用先预编译注册、再编译更新的流程。`ParseLambda` 目前尚未实现。实际执行还需要宿主按使用场景准备 GMLib 的运行时上下文。

## 功能状态

当前代码已包含变量与表达式、条件与循环、函数与构造函数、结构体、注释、自增自减，以及实例和脚本资源的部分集成实现。

仍待完善的内容包括 `with`、数组声明、三元表达式、枚举、宏及部分特殊关键字。具体开发记录见 [TODO.md](TODO.md)；其中的勾选表示开发记录，不代表完整的兼容性测试结论。

仓库目前尚未包含自动化测试项目或独立的可运行示例。构建通过只能确认编译成功，不能证明 GML 行为正确。

## 仓库结构

```text
GMLSharpener/
├── GMLSharpener.sln       # 解决方案
├── GMLSharpener.csproj    # 类库项目与依赖配置
├── Lexer.cs              # Token 定义与词法分析
├── Parser.cs             # 语法解析、表达式树生成与脚本注册
├── RValue.cs             # 运行时值、运算与访问器
├── TODO.md               # 功能开发记录
├── COPYRIGHT.md          # 版权声明
└── LICENSE.md            # GPL 许可证全文
```

## 许可证

GMLSharpener 使用 **GNU GPL v3.0 或更新版本**，详见 [COPYRIGHT.md](COPYRIGHT.md) 和 [LICENSE.md](LICENSE.md)。
