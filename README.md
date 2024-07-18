# PWHL Goalie Tracker 🥅

An interactive dashboard showcasing goalie statistics and team rankings for the inaugural season of the PWHL.

## Motivation

🎯 **Target Audience:** Hockey fans! 

The Professional Women's Hockey League (PWHL) launched its inaugural season in January 2024. First of its kind, it brings together world-ranking players, to compete among six teams located in North America. With high ticket sales and media coverage from the start, the fan support has been impressive—marking a historic moment in women’s sports overall. While this app serves to further promote the PWHL, it shines a spotlight on one of the most crucial, and sometimes overlooked, aspects of the game: goalies! 

By providing a convenient way to interact with goalie statistics, like save percentage and average goals against, along with overall team rankings, users can dive into the depths of the PWHL.  Beyond enhancing the fan experience, this app recognizes the pivotal role of sports statistics in informing coaching decisions and even facilitating engagement in sports betting. 

## App Description

This app allows users to interact with a variety of goaltender statistics, including: save percentage, average goals against per game, shutouts, games played and the amount of wins and losses for each goalie. It also includes the trend in team standings throughout the season.

<div align="center">
  <img src="https://github.com/user-attachments/assets/c1c47901-a3af-4562-bc1a-188032edd1fd" alt="pwhl_goalie_tracker_demo">
</div>

## Usage

🚀 To start using using the app, click here: [PWHL Goalie Tracker](https://2wc8dv-nicole-bidwell.shinyapps.io/pwhl_goalie_tracker/) 

On the dashboard, start your exploration by selecting your preferred teams. Hover over the Team Overall Standings chart to observe team points throughout the season. Compare win-loss statistics for each goalie, or click on a goalie in the table to uncover detailed statistics and see how they stack up against the average!

## Development  

To run the dashboard locally follow the installation steps below.

1. Clone the repository. In your terminal run:

    ```console
    $ git clone https://github.com/nicolebid/pwhl_goalie_tracker.git 
    ```

2. Navigate to the root of the directory: 
    ```console
    $ cd pwhl_goalie_tracker 
    ```
    
3. Open R or Rstudio, and install `renv` if you don't have it: 

    ```console
    $ install.packages("renv")
    ```
4. Install the package dependencies in the `renv.lock` file: 

    ```console
    $ renv::restore()
    ```

5. To run the dashboard, from the root of the directory (`pwhl_goalie_tracker`) run: 

    ```console
    $ shiny::runApp("R")
    ```

6. Click on the displayed link in the output to open the dashboard, if it does not open automatically.

## Contact

If you'd like to suggest a new feature or if you experience a problem while using the dashboard, please open an [issue](https://github.com/nicolebid/pwhl_goalie_tracker/issues). For problems, please include: 
- A description of the problem, indicating details that would be helpful in troubleshooting.
- Detailed steps to reproduce the problem.

## License

📄 PWHL Goalie Tracker is licensed under the MIT license. See [LICENSE.md](LICENSE.md) for details. 

## Attribution

#### 📈 Data Source

Data for the PWHL Goalie Tracker was sourced through [fastRhockey: The SportsDataverse's R Package for Hockey Data](https://fastRhockey.sportsdataverse.org), developed by Ben Howell and Saiem Gilani. The [GitHub released version](https://github.com/sportsdataverse/fastRhockey) was used. Data preprocessing steps can be found [here](R/data_pull_process.R). 
