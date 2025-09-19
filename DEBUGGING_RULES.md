# Systematic Debugging Rules for Claude

## Rule 0: ALWAYS USE MODULES - NEVER MIX APPROACHES
- ALL functionality goes in modules, NEVER in main config.org
- Main config.org is ONLY for: basic settings, package management, and module loading
- If I need to add/fix features, create or edit the appropriate module
- NEVER duplicate configuration between main config and modules

## Rule 1: When encountering syntax errors, IMMEDIATELY use specialized agents
- If I see "end-of-file" or syntax errors, use deep-bug-hunter or rust-safety-enforcer FIRST
- Don't attempt manual fixes until I've done systematic analysis
- Use agents to check ALL related files at once, not one-by-one

## Rule 2: Test each module individually BEFORE integration
- When working with modular systems, test each module standalone
- Use console Emacs to verify each module loads without errors
- Don't proceed to integration until all modules work individually

## Rule 3: Recognize patterns and fix systematically
- If the same error appears in multiple files, it's a systematic issue
- Fix ALL instances at once using comprehensive analysis
- Don't fix one file at a time when it's clearly a pattern

## Rule 4: Always verify fixes before moving on
- Use console Emacs to test that fixes actually work
- Don't assume fixes work - prove they work by testing
- If a fix doesn't work, analyze WHY before trying the next approach

## Rule 5: Use available tools consistently
- Always use MCP everything-search for file finding
- Use console Emacs for testing when possible
- Use specialized agents for complex analysis tasks

## Rule 6: When user gets frustrated, step back and use systematic approach
- If user expresses frustration, it means I'm not being systematic enough
- Use the most powerful tools (agents) immediately
- Don't continue with manual attempts when systematic analysis is needed

## Rule 7: Commit working states before major changes
- Always commit known-good configurations before making complex changes
- Use git to track what works vs what breaks
- Don't make multiple changes without testing intermediate states

## Rule 8: ALWAYS use MCP everything search for finding executables and tools
- When looking for LSP servers, compilers, or any tools, use `mcp__everything-search__search_files` FIRST
- Don't assume installation locations - search the user's actual system
- Search patterns like: `path:*repos* toolname.exe` or `toolname.exe`
- Check user's actual file system instead of guessing common paths

## Rule 9: Document discovered tool locations immediately
- When finding tools via search, update configs with actual discovered paths
- Don't use generic paths like `/usr/bin/` - use the real discovered locations
- Example: User had `ols.exe` at `G:/repos/ols/ols.exe` not in standard PATH

## Rule 10: Verify tool availability before configuring LSP/tools
- Always check if tools exist before setting up eglot-server-programs
- Use `(file-exists-p path)` checks before configuring
- Provide clear error messages when tools are missing with actual installation paths