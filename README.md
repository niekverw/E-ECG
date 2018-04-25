# E-ECG
Method for obtaining reliably measured normal-to-normal RR intervals in exercise ECGs, quality control measurements and plots.

##Further documentation will be added at a later time

### Basic example
run using `Rscript E-ECG.main.R example/example_participants.qrs.csv` to create 1) a file containing heart rate variables in rest, exercise and recovery, 2) a plot (.pdf) that shows the RR-interval in time for inspection:



![RR](example.jpg "example RR interval profile")


*pr = rest phase (before exercise starts), ex= exercise phase , re = recovery phase.*


### Variables names of interest
- maxmets - Maximum achieved exercise workload
- DurationECG - Total duration of the ecg. 
- RR_[pr,ex,re]_duration - Total duration of each phase 
- RR_pr_mean - Resting heart rate (mean heart rate in pretest)
- RR_ex_min - Peak RR during exercise
- RR_re_mean3s[10..50] - Heart rate during the recovery phase at [10..50] seconds.
- RR_[pr,ex,re]_slidingSdSdTsc - Standard deviation over a rolling standard deviation with a window of 3 beats; used in outlier detection explained in more detail below 

#### ECG Outlier/noise detection
Adjacent normal-to-normal RR intervals are expected to be proximal to each-other, therefore a standard deviation (of a rolling standard deviation in 3 beat windows) near zero suggests little to no deviation in the RR detection, whereas severe deviation from zero strongly indicates an aberrant detection of RR peaks. The median standard deviation was 0.0065 seconds, an extreme value of >0.05 seconds (98%th percentile) was considered to be excess noise and confirmed by manually inspection of the RR intervals.

#### References
- https://www.nature.com/articles/s41467-018-03395-6
- http://jaha.ahajournals.org/content/7/8/e008341
