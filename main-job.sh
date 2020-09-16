#PBS -l nodes=1:ppn=10
#PBS -l walltime=23:59:59

module load r/3.6.1
cd /nics/d/home/jrosenb8/ngsschat
/sw/cs400_centos7.3_acfsoftware/r/3.6.1/centos7.3_gnu6.3.0/bin/Rscript /nics/d/home/jrosenb8/ngsschat/hpc-run.R