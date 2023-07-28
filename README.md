# Cape_Run_Values
Calculate Run Values at both the Pitch and PA level for the Cape Cod League

ReadPBPData.R reads in and merges together play-by-play data from the last 11 seasons of the Cape League. Thank you to Chris Thoms for providing the data!

RunValuePBP.R analyzes the run environment of the Cape Cod League to calculate the run value of all possible events at the Plate Appearance and Pitch Level. It also calculates the value of running events. Then, calculates wOBA constants for the Cape League. Finally, builds a RE288 table with run impacts of incorrect calls for use in the Ump Scorecard

RunValueTrackman.R calculates run values from a Trackman file, and RunValuePlots.R generates plots of run values by pitch type and location from a Trackman file


