# formal-chemistry-Haskell

一个用 Haskell 编写的化学编译器原型。目前支持 Zn、Fe、Cu 与 HCl、H2SO4、相关盐之间的基础反应分析，并正在加入基于元素守恒矩阵的自动配平。

## 处理流程

```text
Source -> Lexer -> Parser -> Analyzer -> Evaluator -> Render
                                  |
                                  v
ResolvedReaction -> Counter -> BalanceIR -> Solver
```

`BalanceIR` 保存元素基、物种基和整数化学计量矩阵。`Solver` 将矩阵交给独立 Math 模块，通过精确的有理数高斯消元求解 `Mx = 0`，最终返回最简整数系数。

## 主要目录

- `Chemistry/Definitions/`：Token、AST、领域类型和各阶段中间表示。
- `Chemistry/Workers/`：Lexer、Parser、Analyzer、Evaluator、Counter、IRBuilder、Solver 和 Render。
- `Chemistry/Math/`：不依赖化学类型的矩阵与 kernel 求解算法。
- `Chemistry/Interface/`：编译流程对外入口。
- `test/`：各模块的独立测试程序。

## Solver 核心函数

- `findKernel`：把整数矩阵转换为有理数矩阵，完成消元、回代和整数规范化。
- `matrixDealer`：逐列选择主元；主元为零时从剩余行中选取并交换。
- `getZero`：使用当前主元行消去所有后续行的对应位置。
- `disappear`：完成两个行向量之间的一次具体消元。
- `kernelFromEchelon`：从阶梯矩阵选择自由变量并反向回代。

当前可靠支持零维和一维 kernel。对于更高维 kernel，暂时返回令第一个自由变量为 `1`、其余自由变量为 `0` 时得到的一个有效向量，而不是完整的 kernel 基。

## 运行测试

当前 Cabal 配置仍处于禁用状态，可以直接使用 `runghc`：

```powershell
runghc --ghc-arg=-i. test/TestGaussianElimination.hs
runghc --ghc-arg=-i. test/TestIRBuilder.hs
runghc --ghc-arg=-i. test/TestCompiler.hs
```

最近一次结果：Solver/Math 5/5、IRBuilder 3/3、编译器集成测试 17/17。

## 分支说明

- `main`：当前主线，包含编译器流水线、BalanceIR 和 kernel Solver。
- `agent/project-restructure-main`：IR、Counter 和 Solver 的开发分支，内容已合并到 `main`。
- `agent/project-restructure`：早期目录重构实验。
- `agent/import-modified-project`：更早的工程导入快照。

目前 Solver 尚未接入 `compileReaction` 总入口；下一步可以把 `Evaluator -> ResolvedReaction -> BalanceIR -> Solver` 串入最终渲染流程。
