import pycmsaf.ecf_utils as ecf_utils
import datetime
import ecflow

if __name__ == '__main__':
    ci = ecf_utils.connect(host='ecgb11', port=35818)
    ci.sync_local()
    defs = ci.get_defs()

    years = range(1982, 2014+1)
    months = range(1, 12+1)
    for year in years:
        for month in months:
            for task in ('grib2ncdf', 'simulator'):
                path = '/CLOUD_SIMULATOR/processing/{0}/{1:02d}/{2}'.format(year, month, task)
                ci.force_state(path, ecflow.State.complete)


