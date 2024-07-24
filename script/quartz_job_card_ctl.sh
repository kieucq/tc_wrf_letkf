#!/bin/bash
#
# NOTE: This is to create a job card for torque scheduler
#
NSLOTS=$1
rundir=$2
if  [ "$NSLOTS" == "" ] || [ "$rundir" == "" ]; then
    echo "tc_job_card_pbs.sh script cannot be run with NSLOTS = $NSLOTS"
    exit 1
fi
cat > wrf_qsub.sh << EOF1
#!/bin/bash -l
#SBATCH -N 1
#SBATCH -n 64
#SBATCH -t 03:00:00
#SBATCH -J wrfletkf
#SBATCH -A r00296
#SBATCH --ntasks-per-node=128
#SBATCH --mem=128GB
#module unload xalt
ulimit -s unlimited
cd $rundir
ulimit -s unlimited
mpirun -n 64 ./wrf.exe
#srun -n 32 ./wrf.exe
#srun ./wrf.exe
#rm -f *.exe *.TBL *DATA* ozone* *.tbl grib* co2_trans*
EOF1

