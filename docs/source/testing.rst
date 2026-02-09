===============
Testing Changes
===============

System Configuration Testing
============================

1. **Validate module syntax** (fast, no builds):

   .. code-block:: bash

       ./scripts/validate-refactor.sh

2. **Test system build** (dry-run, no sudo needed):

   .. code-block:: bash

       guix time-machine -C channels.scm -- system build -L . \
         entelequia/system/machines/einstein.scm --dry-run

3. **Test in VM** (safest, isolated environment):

   .. code-block:: bash

       # Desktop VM test
       ./scripts/test-vm.sh test-desktop

       # Server VM test
       ./scripts/test-vm.sh test-server

4. **Dry-run on actual hardware** (shows changes without applying):

   .. code-block:: bash

       sudo guix time-machine -C channels.scm -- \
         system reconfigure entelequia/system/machines/einstein.scm --dry-run

5. **Apply to actual hardware**:

   .. code-block:: bash

       sudo guix time-machine -C channels.scm -- \
         system reconfigure entelequia/system/machines/einstein.scm

6. **Rollback if needed**: ``sudo guix system roll-back``

Home Environment Testing
========================

1. **Test home build**: ``guix home build entelequia/home/home-config.scm``
2. **Dry-run**: ``guix home reconfigure --dry-run entelequia/home/home-config.scm``
3. **Apply changes**: ``guix home reconfigure entelequia/home/home-config.scm``
4. **Rollback if needed**: ``guix home roll-back``

Best Practice Testing Workflow
===============================

For major changes, follow this sequence:

1. Run ``./scripts/validate-refactor.sh`` (syntax check)
2. Test in VM with ``./scripts/test-vm.sh test-desktop``
3. Dry-run on actual hardware (laptop first, easier to rollback)
4. Deploy to laptop (curie), test for 24-48 hours
5. Deploy to desktop (einstein) after laptop proves stable
