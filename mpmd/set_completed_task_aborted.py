#!/usr/bin/env python2.7

# NOTE: before executing this script, you have to: 
#   source ../config.sh 
#   for having the right ECF_NODE and ECF_PORT

import ecflow

task_list = ['grib2ncdf', 'simulator']

client = ecflow.Client()
client.sync_local()
defs = client.get_defs()
tasks = defs.get_all_tasks()

for task in tasks:
    t = task.get_abs_node_path()
    tt = t.split("/")[-1]
    if tt in task_list: 
        if task.get_state() == ecflow.DState.complete:
            print " ** Working on: ", task.get_abs_node_path(), task.get_state()
            client.force_state(task.get_abs_node_path(), ecflow.State.aborted)

