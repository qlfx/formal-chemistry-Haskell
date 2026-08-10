# formal-chemistry-Haskell

一个用 Haskell 构造的最小化学领域编译器原型。目前聚焦金属与酸、金属与盐两类反应，目标是逐步形成清晰的编译流水线：

```text
Source -> Lexer -> Token -> Parser -> AST -> Semantic Analysis -> Evaluation -> Render
```

## 目录

- `src/Chemistry/Definitions/`：领域类型、Token、AST、语义中间表示与错误类型。
- `src/Chemistry/Workers/`：词法分析、语法分析、语义分析、求值、渲染与总编译入口。

## 当前完成的内容

- 化学式 AST：元素、下标、括号基团、反应物列表与计量系数。
- 词法层：元素符号、整数、`+`、括号与 EOF Token。
- 递归下降 Parser：`species ('+' species)+`，支持元素下标和嵌套括号基团。
- 语义分类：识别 Zn/Fe/Cu、HCl/H2SO4，以及对应氯化物和硫酸盐。
- 反应规则：金属活动性比较，金属-酸与金属-盐反应是否发生及产物推导。
- 输出渲染：把反应结果重新渲染成化学式字符串。

## 当前状态

这批源文件记录了模块分层重构过程中的阶段性快照，尚未形成可编译版本：

- `Lexer.hs` 和 `Parser.hs` 仍使用旧模块名及旧导入路径；文件已按职责归入 `Workers`，但源码中的模块声明尚待统一。
- `Compiler.hs` 尚未把 Lexer 接入总流水线，当前把 `String` 直接交给需要 `[Token]` 的 Parser。
- Parser、Analyzer 使用不同的 `Either` 错误类型，总入口需要统一错误封装。
- `Compiler.hs` 导入 `renderReactionResult`，但调用了尚未定义的 `renderResult`。
- 目前尚缺构建配置、可执行入口和自动化测试。

## 近期计划

1. 统一所有模块路径为 `Chemistry.Definitions.*` 与 `Chemistry.Workers.*`。
2. 打通 `lexer -> parser -> analyzer -> evaluator -> render`。
3. 设计统一的编译错误类型，并为错误附加源位置。
4. 添加 Cabal/Stack 配置、`Main` 与分层测试。
5. 在稳定的最小系统上加入元素守恒、配平和更一般的化学规则 IR。

## 长期方向

- 将项目发展为化学 DSL 与可解释规则系统。
- 用 Z3 处理约束求解和配平。
- 用 Lean 逐步形式化 Parser 不变量、元素守恒和规则正确性。
