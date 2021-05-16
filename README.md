# DP Rankings App

## Main goals:

> -   a drag and drop interface for reranking players [Done!]
>
> -   instantly recolour to provide context about your ranking vs consensus [Done!]
>
> -   download your rankings to excel/csv [Done!]
>
> -   work on authentication so that you can login and retrieve old rankings, ideally hooking into a service like Auth0 to allow for self-serve signup/logins/third-party logins <https://github.com/curso-r/auth0> [Done!]
>
> -   integrate current DynastyProcess app styling c/o bs4Dash [Done!]
>
> -   save your rankings to a database, encoded with a user's ID [database? experiment with mongo?] [Done! - Arrow]
>
> -   BUG: switch to proxy updating so that it doesn't regenerate the table [Done!]
> -   deploy to server [Done!]
> -   automatic/weekly sync to latest DP data c/o GH [Done!]
> -   Add rookie data [Done!]
> -   minor code cleanups and refactors [Done!]
> -   tab/interface for retrieving data [Done!]
> - recalculating standard deviation for players lower than a certain threshold [Done!]
> - reorder columns
> - Add meta tags (c/o metathis package) [Done!]
> - Add age column [Done!]
> - Import session ID (rename as rankings ID) as a template for making new rankings
> IF my_history THEN show single or up to three players, plotting Z score over time, X = TIME, Y = Z [Done!]
> - Load Rankings - have buttons fill box and session ID appear below [Done!] 
> - Main panel's boxes have right hand whitespace [Done!]
> - Top two inputs on main panel create changes downstream when they should not [Done!]
> - Create a plot to show/highlight players with the highest absolute Z score (i.e. to highlight highest/lowest players compared to average) [Done!]
> - IF session_id THEN show bar plot with x = Z, y = player of top five and bottom five (and allow filtering for specific players) [Done!]
> - Sticky headers in table - scrollY? [Not Implemented]
> - Data automatic updater - check why cron did not run
> - Ranking ID - stretch full horizontal
> - About box or tab for section [inputs, drag/drop overlay]?
>   - Z score

