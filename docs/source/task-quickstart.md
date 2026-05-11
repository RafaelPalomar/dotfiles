# Task Management System - Quick Start

**5-Minute Setup & Usage Guide**

## 🚀 Quick Setup

### 1. Reload Everything
```bash
# Reload sxhkd (hotkeys)
killall -SIGUSR1 sxhkd

# Reload Polybar (status bar)
~/.config/polybar.local/launch.sh

# In Emacs: reload config
M-x load-file RET ~/.config/emacs/init.el RET
# OR restart Emacs
```

### 2. Test Basic Capture
```bash
# Press: super + c
# Type: "Test task"
# Select: work
# ✅ You should see a notification
```

## 🎯 Essential Hotkeys

| Hotkey | Action |
|--------|--------|
| `super + c` | **Quick capture** (fastest way to capture) |
| `super + shift + c` | Capture menu (more options) |
| `super + shift + a` | Open agenda |
| `super + shift + i` | **Clock in/out** |
| `super + shift + n` | Quick note (Denote) |
| `super + shift + g` | Sync GitHub issues |

## 📊 Polybar Integration

**Look at your Polybar** - you should see:
- `org-clock` module (left of filesystem icon)
- When clocked in: shows task name and time
- When clocked out: shows nothing

**Click actions**:
- **Left click**: Jump to current task in Emacs
- **Middle click**: Clock control menu
- **Right click**: Clock out

## 📝 Daily Workflow (30 seconds)

### Morning (10 sec)
```
super + shift + a  → See today's tasks
super + shift + i  → Clock In on first task
```

### During Day (5 sec each)
```
super + c  → Capture quick ideas/tasks
           → Keep working, clock running in Polybar
```

### Evening (15 sec)
```
super + shift + i  → Clock Out
super + shift + a  → Review what you completed
```

## 🎨 Capture Menu Options

Press `super + shift + c` to see:

1. **Quick Task** → Basic TODO
2. **Detailed Task** → Opens Emacs for detailed entry
3. **Project Idea** → Captures project with template
4. **Meeting Note** → Meeting template (agenda, attendees, actions)
5. **AI Task** → Structured task for AI collaboration
6. **GitHub Issue** → Capture specific issue from GitHub
7. **Quick Note** → Denote note (for knowledge base)

## 🤖 AI-Assisted Tasks (Advanced)

### Create AI-Friendly Task
1. `super + shift + c` → AI Task
2. Fill in:
   - Task description
   - CONTEXT (what's the situation)
   - EXPECTED_OUTPUT (what you want)
   - FILES (relevant file paths)
3. In the `#+BEGIN_AI_CONTEXT` block, add detailed context

### Use with AI
1. In Emacs, navigate to your AI task
2. Press `SPC m a x` → Copies formatted task to clipboard
3. Paste into GPtel/Claude
4. AI has all context needed!

## 📂 Where Things Go

```
~/org/
├── inbox.org              ← Your captured work tasks
├── inbox-personal.org     ← Your captured personal tasks
├── projects.org           ← Projects created with SPC p n
├── github-issues.org      ← Synced GitHub issues
├── weekly-review.org      ← Weekly review notes
└── SYSTEM_README.org      ← Complete documentation
```

## ⚙️ Optional: GitHub Sync

### One-time setup
```bash
# Edit this file:
nano ~/.config/org-github-repos

# Add your repos (one per line):
RafaelPalomar/dotfiles
owner/another-repo
```

### Use
```bash
# Manual sync:
super + shift + g

# View synced issues:
super + shift + a  (they appear in agenda)
```

## 🔧 Emacs Commands (Inside Emacs)

### Navigation
- `SPC n a` → Open agenda
- `SPC n n` → Capture new item

### Clock
- `SPC n ci` → Clock in
- `SPC n co` → Clock out
- `SPC n cr` → Resume last clocked task
- `SPC n cg` → Go to current clocked task
- `SPC n cR` → Today's clock report

### Projects
- `SPC p n` → Create new project
- `SPC p k` → Show project Kanban board

### Weekly Review
- `SPC n w` → Open 3-pane review layout
  - Left: Week agenda
  - Top-right: Clock report
  - Bottom-right: Review notes

## 🎓 Learning Path

### Day 1: Basic Capture
- Use `super + c` for every task/idea
- Don't organize yet, just capture

### Day 2: Time Tracking
- `super + shift + i` → Clock In when starting work
- Watch Polybar show your progress
- Clock out when switching tasks

### Day 3: Agenda
- `super + shift + a` → Review your captured items
- See how super-agenda groups them
- Mark some done: `t` (in agenda)

### Week 1: Projects
- Create a project: `SPC p n`
- Break it into tasks
- Clock time on each task
- `SPC p k` to see Kanban view

### Week 2: AI Integration
- Try AI task: `super + shift + c` → AI Task
- Export to AI: `SPC o t x`
- Get AI help with complex tasks

## 🚨 Troubleshooting

### "Capture doesn't work"
```bash
# Test emacsclient
emacsclient -e '(message "test")'
# Should return: "test"

# If no Emacs running:
emacs --daemon
```

### "Polybar shows nothing"
```bash
# Test clock status script
~/.local/bin/org-clock-status.sh
# Should return empty if no clock running

# Start a clock in Emacs:
super + shift + i → Clock In
```

### "GitHub sync fails"
```bash
# Check gh is authenticated
gh auth status

# Should show: Logged in to github.com
```

## 📚 Full Documentation

- **User Guide**: `~/org/SYSTEM_README.org`
- **Implementation Details**: `~/.dotfiles/TASK_MANAGEMENT_IMPLEMENTATION.md`
- **Source Config**: `~/.dotfiles/emacs.org`

## 💡 Pro Tips

1. **Capture Liberally**: Use `super + c` for everything. Refine later.
2. **Clock Everything**: Builds awareness of time spent
3. **Weekly Reviews**: `SPC n w` every Sunday/Monday
4. **Link Notes**: Use Denote for detailed notes, link to tasks
5. **AI for Breakdown**: Complex task? `SPC o t b` for AI breakdown
6. **GitHub Integration**: Keep issues and org-mode in sync
7. **Backup**: Automatic at 23:00, but run manually first

## ✅ Success Indicators

You're using it well when:
- You capture >10 tasks/day
- You know what you worked on yesterday (clock report)
- Your agenda has <5 overdue items
- You do weekly reviews consistently
- You rarely forget tasks (captured immediately)

---

**Need Help?** Check `~/org/SYSTEM_README.org` for complete guide!
