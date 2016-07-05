#!/usr/bin/env python2.7
from pycmsaf.logger import setup_root_logger

logger = setup_root_logger(name='root')

import sys
import ecflow
import argparse
import datetime
import calendar
from config_suite import *
from dateutil.rrule import rrule, MONTHLY
from pycmsaf.argparser import str2date
from pycmsaf.utilities import date_from_year_doy
from pycmsaf.ssh_client import SSHClient


def set_vars(suite):
    """
    Set suite level variables
    """
    # suite.add_variable('TURTLES', 'I like turtles')
    suite.add_variable("ECF_MICRO", "%")

    # Location of the MPMD client:
    suite.add_variable("MPMD_CLIENT", mpmd_client_prog)

    # Specify the python interpreter to be used:
    suite.add_variable("PYTHON", "PYTHONPATH=$PYTHONPATH:" +
                       perm + " " + python_path)

    # Directory on the remote machine, where all generated 
    # files from "ECF_HOME" will be copied before execution
    suite.add_variable("REMOTE_HOME", remote_home_dir)

    # Directory on the remote machine, 
    # where all jobs write their output
    suite.add_variable("REMOTE_LOGDIR", remote_log_dir)

    # Remote user and host names
    suite.add_variable("REMOTE_USER", remote_user_name)
    suite.add_variable("REMOTE_HOST", remote_host_name)

    # Network settings for communication with MPMD database:
    suite.add_variable("MPMD_DATABASE", mpmd_database)
    suite.add_variable("MPMD_SUBMIT_SCRIPT", mpmd_submit_script)
    suite.add_variable("MPMD_MODE", mpmd_mode)
    suite.add_variable("MPMD_SUBMIT_CMD", mpmd_submit_cmd)

    # Standard ecflow variables:
    suite.add_variable("SUITE_HOME", suite_home_dir)
    suite.add_variable("ECF_HOME", ecf_home_dir)
    suite.add_variable("ECF_FILES", ecf_files_dir)
    suite.add_variable("ECF_INCLUDE", ecf_include_dir)
    suite.add_variable("ECF_OUT", ecf_out_dir)

    # Miscellaneous:
    suite.add_variable("ECF_TRIES", '1')
    suite.add_variable("ECF_SUBMIT", ecflow_submit)
    suite.add_variable("ECFS_PATH", ecfs_path)
    suite.add_variable("TAR_PREFIX", tar_prefix)
    suite.add_variable("TAR_SUFFIX", tar_suffix)
    suite.add_variable("THV", thv)
    suite.add_variable("MPC", mpc)
    suite.add_variable("SCOPS", scops)
    suite.add_variable("OVERLAP", overlap)
    suite.add_variable("CRUN", crun)
    suite.add_variable("SSTFILE", sstfile)
    suite.add_variable("INPUT", input)
    suite.add_variable("OUTPUT", output)
    suite.add_variable("PROG", prog)


def add_trigger(node, trigger):
    """
    Make a given node wait for the trigger node to complete.
    @param node: The node that has to wait.
    @param trigger: The trigger node.
    @type node: ecflow.[family/task]
    @type trigger: ecflow.[family/task]
    @return: None
    """
    node.add_trigger('{0} == complete'.
            format(trigger.get_abs_node_path()))


def add_fam(node, fam):
    """
    Make new family for given node.
    """
    new_fam = node.add_family(fam)
    return new_fam

def add_mpmd_task(family, taskname):
    task = family.add_task(taskname)
    for event in ('mpmd_queued', 'mpmd_submitted', 'mpmd_error'):
        task.add_event(event)
    return task


def add_dearchiving_task(family):
    family.add_task('get_era_data')


def add_archiving_task(family):
    family.add_task('put_sim_data')


def add_mpmd_tasks(family):
    grib2ncdf = add_mpmd_task(family, 'grib2ncdf')
    simulator = add_mpmd_task(family, 'simulator')

    add_trigger( simulator, grib2ncdf )

    return dict( grib2ncdf=grib2ncdf, simulator=simulator )


def familytree(node, tree=None):
    """
    Given an ecflow node, walk the tree in downward direction 
    and collect all families.

    @param node: The node to start from.
    @type node: ecflow.Node
    @return: All collected families
    @rtype: list
    """
    # Initialize tree on the first call
    if tree is None:
        tree = list()

    # Walk the current node's subnodes
    subnodes = node.nodes
    for subnode in subnodes:
        # Save node to the tree if its type is ecflow.Family
        if isinstance(subnode, ecflow.Family):
            abspath = subnode.get_abs_node_path()
            tree.append(abspath[1:])  # skip initial '/' because otherwise
            # os.path.join() doesn't join paths
            # correctly.

            # Call function recursively.
            familytree(subnode, tree)

    return tree


def build_suite():
    """
    Build the ecflow suite.
    """
    logger.info('Building suite.')

    # ========================
    # GENERAL SUITE PROPERTIES
    # ========================

    defs = ecflow.Defs()
    suite = defs.add_suite(mysuite)

    # Set suite level variables
    set_vars(suite)

    # Set default status
    suite.add_defstatus(ecflow.DState.suspended)

    # Define thread limits
    suite.add_limit("mpmd_threads", mpmd_threads_number)
    suite.add_limit("serial_threads", serial_threads_number)

    # ========================
    # ADD CRON JOB
    # ========================

    start = ecflow.TimeSlot(0, 0)
    finish = ecflow.TimeSlot(23, 59)
    incr = ecflow.TimeSlot(0, 1)
    time_series = ecflow.TimeSeries(start, finish, incr, False)
    cron = ecflow.Cron()
    cron.set_time_series(time_series)
    fam_submit = suite.add_family( 'queue_submitter' )
    submit = fam_submit.add_task( 'submit' )
    submit.add_cron(cron)
    fam_submit.add_variable( 'ECF_JOB_CMD', ecgate_job_cmd )

    # ========================
    # DEFINE TOPLEVEL FAMILIES
    # ========================

    fam_dearch = suite.add_family( 'dearchiving' )
    fam_proc = suite.add_family( 'processing' )
    fam_arch = suite.add_family( 'archiving' )

    # Activate thread limits
    fam_dearch.add_inlimit( 'serial_threads' )
    fam_proc.add_inlimit( 'mpmd_threads' )
    fam_arch.add_inlimit( 'serial_threads' )

    # Define job commands
    fam_dearch.add_variable( 'ECF_JOB_CMD', serial_job_cmd )
    fam_proc.add_variable( 'ECF_JOB_CMD', mpmd_job_cmd )
    fam_arch.add_variable( 'ECF_JOB_CMD', serial_job_cmd )

    # ===============================
    # DEFINE DYNAMIC FAMILIES & TASKS
    # ===============================

    for mm in rrule(MONTHLY, dtstart=args.sdate, until=args.edate):

        yearstr    = mm.strftime("%Y")
        monthstr   = mm.strftime("%m")
        act_date   = datetime.date(int(yearstr), int(monthstr), 1)
        first_day  = "01"
        last_day   = calendar.monthrange(int(yearstr),int(monthstr))[1]
        yyyymm     = yearstr + monthstr
        start_date = yearstr + monthstr + first_day 
        end_date   = yearstr + monthstr + str(last_day)

        if args.ignore_months:
            if int(monthstr) in args.ignore_months:
                continue

        try:
            # dearchiving family
            fam_year_dearch = add_fam( fam_dearch, yearstr )

            # processing family
            fam_year_proc = add_fam( fam_proc, yearstr )
            add_trigger( fam_year_proc, fam_year_dearch )

            # archiving family
            fam_year_arch = add_fam( fam_arch, yearstr )
            fam_year_arch.add_variable( "YEAR", yearstr )
            add_archiving_task( fam_year_arch )
            add_trigger( fam_year_arch, fam_year_proc )

        except RuntimeError:
            pass

        # dearchiving family
        fam_month_dearch = add_fam( fam_year_dearch, monthstr )
        fam_month_dearch.add_variable( "YYYYMM", yyyymm )
        fam_month_dearch.add_variable( "START_DATE", start_date )
        fam_month_dearch.add_variable( "END_DATE", end_date )
        fam_month_dearch.add_variable( "NDAYS", last_day )
        add_dearchiving_task( fam_month_dearch )

        # processing family
        fam_month_proc = add_fam( fam_year_proc, monthstr )
        fam_month_proc.add_variable( "YYYYMM", yyyymm )
        fam_month_proc.add_variable( "SY", yearstr )
        fam_month_proc.add_variable( "EY", yearstr )
        fam_month_proc.add_variable( "SM", monthstr )
        fam_month_proc.add_variable( "EM", monthstr )
        fam_month_proc.add_variable( "SD", first_day )
        fam_month_proc.add_variable( "ED", last_day )
        add_mpmd_tasks( fam_month_proc )
        #add_trigger( fam_month_proc, fam_month_dearch )


    # ============================
    # CREATE SUITE DEFINITION FILE
    # ============================

    # Check job creation
    defs.check_job_creation()

    # Save suite to file
    suite_def_file = mysuite + '.def'
    logger.info('Saving suite definition to file: {0}'.
            format(suite_def_file))
    defs.save_as_defs(suite_def_file)

    # ======================
    # CREATE LOG DIRECTORIES
    # ======================

    logger.info('Creating log directories on both the local and '
                'the remote machine.')

    # Create a tree of all families in the suite 
    # (i.e. families, subfamilies, subsubfamilies etc)
    tree = familytree(suite)

    # Create corresponding log-directory tree:
    # 1.) Local machine
    for node in tree:
        dirname = os.path.join(ecf_out_dir, node)
        if not os.path.isdir(dirname):
            os.makedirs(dirname)

    # 2.) Remote machine
    ssh = SSHClient(user=remote_user_name, host=remote_host_name)
    for node in tree:
        remote_dir = os.path.join(remote_log_dir, node)
        ssh.mkdir(remote_dir, batch=True)  # batch=True appends this mkdir
        # call to the command batch.

    # Create all remote directories in one step (is much faster)
    ssh.execute_batch()


if __name__ == '__main__':

    parser = argparse.ArgumentParser(
        description=sys.argv[0] + " creates the suite " 
        + mysuite + " required by ecflow.")

    parser.add_argument('--sdate', type=str2date, required=True, 
                        help='start date, e.g. 20090101')
    parser.add_argument('--edate', type=str2date, required=True, 
                        help='end date, e.g. 20091231')
    parser.add_argument('--ignore_months', type=int, nargs='*', 
                        help='e.g. 1 2 5 7 11 12')

    args = parser.parse_args()

    logger.info("\n")
    logger.info("Script     : %s" % sys.argv[0])
    logger.info("start date : %s" % args.sdate)
    logger.info("end date   : %s" % args.edate)
    logger.info("ign. months: %s" % args.ignore_months)
    logger.info("Creating suite definition %s" % mysuite)
    logger.info("\n")

    build_suite()

    logger.info("SCRIPT {0} finished".format(os.path.basename(__file__)))
