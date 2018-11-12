
# cloud_simulator

  * **git clone** https://github.com/martinstengel/simfera.git
  
  * cd cloud_simulator/fortran
  
  * ./compile_src.sh 
  
  * configure **job_pbs.cmd** and **qsub job_pbs.cmd**
  
  * or use ECflow on ecgate (see below)


You have to clone this repository twice if you want to use **mpmd**:

    1) local machine: user@ecgate:/path/to/repo/cloud_simulator

    2) remote machine: user@cca/path/to/repo/cloud_simulator


**./mpmd/**

    This is the source code for ecflow required on local machine

    edit config.sh
        adapt all variables (ECF_PORT, ECF_NODE, SUITE, etc.) and paths!

    start ecflow server
        ecflow_start -p <ECF_PORT> -d $HOME/ecflow_logs

    stop ecflow server, if you do not need it anymore!
        ecflow_stop -p <ECF_PORT>

    create mpmd database
        ./createdb.sh    

    edit suite config file
        cd ecflow_suite/
        edit config_suite.py
        **Note**: mpmd_threads_number must be greater equal LENGTH defined in config.sh

    generate suite definition
        ./create_suite.py -h
        ./create_suite.py --sdate 19820101 --edate 20141231

    register and load suite
        cd ..
        [./reset.sh]
        ./register.sh
        ./load.sh

    open ecflowview GUI: ecflowview &
    and resume suite
