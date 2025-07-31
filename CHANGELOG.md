# 更新日志

## [0.3.0] - 2025-01-23
### 新功能 🎉
- 添加了智能导航功能：`org-luhmann-next-unnumbered-heading` 和 `org-luhmann-previous-unnumbered-heading`
- 新增导出功能：`org-luhmann-export-region-as-links`，支持将选中区域的标题导出为 org-mode 链接
- 集成了 savehist-mode 支持，自动保存导出历史记录
- 添加了智能文件处理：自动创建目录，支持追加到现有文件或创建新文件

### 改进 🔧
- 精简了快捷键映射，移除了冗余功能
- 改进了文件选择交互，使用 `read-file-name` 提供更好的用户体验
- 优化了代码结构，移除了不必要的功能

### 移除 🗑️
- 移除了 `org-luhmann-next-number` 和 `org-luhmann-previous-number` 导航功能
- 移除了 `org-luhmann-reorder` 重新排序功能
- 移除了 `org-luhmann-goto-number`、`org-luhmann-list-numbers`、`org-luhmann-export-index` 等冗余功能
- 移除了 `org-luhmann-insert-between` 插入功能

### Bug 修复 🐛
- 修复了 `read-file-name` 参数类型错误
- 清理了与已删除功能相关的残留代码和提示信息

## [0.2.0] - 2025-01-22
### 新功能 🎉
- 添加了 Luhmann 编号显示增强功能，支持隐藏 org-mode 星号
- 集成了编号显示功能到主包中
- 添加了编号显示样式的自定义选项

## [0.1.0] - 2025-01-21

