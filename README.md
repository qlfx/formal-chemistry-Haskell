# formal-chemistry-Haskell

一个用 Haskell 构造的最小化学领域编译器原型。目前聚焦金属与酸、金属与盐两类反应，采用分层编译流水线：

```text
Source -> Lexer -> Token -> Parser -> AST -> Semantic Analysis -> Evaluation -> Render
```

## 目录结构

- `Chemistry/Definitions/`：领域类型、Token、AST、语义中间表示与错误类型。
- `Chemistry/Workers/`：词法分析、语法分析、语义分析、求值、渲染与总编译入口。
- `test/`：Lexer、Parser、前端流水线、Evaluator 与 Render 的手写测试程序。
- `Main.hs`：当前实验入口。
- `app/Main.hs`：Cabal 初始化生成的占位入口。

## 本次版本已完成

- 所有核心模块已统一到 `Chemistry.Definitions.*` 与 `Chemistry.Workers.*` 命名空间。
- Lexer 能识别 Zn、Fe、Cu、H、O、S、Cl、数字、`+`、括号和 EOF，并返回词法错误位置。
- 递归下降 Parser 支持计量系数、元素下标、括号基团及两个以上反应物。
- Analyzer 能将公式分类为金属、酸或盐，并识别金属—酸、金属—盐反应。
- Evaluator 根据 `Zn > Fe > H > Cu` 活动性顺序判断反应并推导产物。
- Render 能把求值结果转换回化学反应字符串。
- 新增独立的 Lexer、Parser、前端、Evaluator 和 Render 测试文件。

## 当前状态与已知问题

该版本比此前的重构快照更完整，但仍不是可直接构建的发布版本：

- `Chemistry/Workers/Compiler.hs` 尚未在 Parser 前调用 Lexer，并仍调用未定义的 `renderResult`。
- Compiler 的 Parser、Analyzer 错误类型尚未统一到总错误类型。
- 根目录 `Main.hs` 仍引用重构前的 `Chemistry.Parser`、`Chemistry.Compiler` 和不存在的 `parseInputAST`。
- `Haskell-projects.cabal.disabled` 与 `hie.yaml.disabled` 是旧配置的禁用副本，模块清单尚未跟随新命名空间更新。
- 测试目前是多个独立的 `Main` 程序，还没有统一测试框架或 CI。

## 建议的下一步

1. 打通 `lexer -> parser -> analyzer -> evaluator -> render` 的 Compiler 总入口。
2. 设计统一的 `CompilerError`，封装词法、语法和语义错误。
3. 更新并启用 Cabal 配置，明确 `src`、`app` 和 `test` 的目录约定。
4. 修复 `Main.hs`，加入可交互 REPL 或命令行入口。
5. 将现有测试整合为可自动运行的测试套件，并加入 CI。
6. 在稳定的最小系统上增加元素守恒、配平及更一般的反应规则 IR。

## 长期方向

- 将项目发展为化学 DSL 与可解释规则系统。
- 用 Z3 处理约束求解和方程式配平。
- 用 Lean 形式化 Parser 不变量、元素守恒和规则正确性。
