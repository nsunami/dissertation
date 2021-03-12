Qualtrics.SurveyEngine.addOnload(function () {
    /*Place your JavaScript here to run when the page loads*/

    // Hide the textbox first
    jQuery('.InputText').hide();

    // Hide the previous button
    this.hidePreviousButton()
    this.hideNextButton()

});

Qualtrics.SurveyEngine.addOnReady(function () {
    /*Place your JavaScript here to run when the page is fully displayed*/
    var thispage = this;

    // When the start timer button is pressed
    jQuery('#button').on('click', () => {
        // Hide the button
        jQuery('#button').hide();
        // Show the text box question
        jQuery('.InputText').show();
        // Start Timer
        // Set the date we're counting down to
        //var countDownDate = Date.now() + 180000;
        var countDownDate = Date.now() + 180000;

        // Color the text green as the timer starts
        jQuery('#demo').css('color', 'green');

        // Update the count down every 1 second
        var x = setInterval(function () {

            // Get today's date and time
            var now = new Date().getTime();

            // Find the distance between now and the count down date
            var distance = countDownDate - now;

            // Time calculations for days, hours, minutes and seconds
            var days = Math.floor(distance / (1000 * 60 * 60 * 24));
            var hours = Math.floor((distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
            var minutes = Math.floor((distance % (1000 * 60 * 60)) / (1000 * 60));
            var seconds = Math.floor((distance % (1000 * 60)) / 1000);

            // Output the result in an element with id="demo"
            document.getElementById("demo").innerHTML = minutes + "m " + seconds + "s ";

            // If the count down is over, write some text
            if (distance < 0) {
                clearInterval(x);
                thispage.showNextButton();
                jQuery("#demo").html("0m 0s ");
            }
        }, 1000);

        //------------


    });

});

Qualtrics.SurveyEngine.addOnUnload(function () {
    /*Place your JavaScript here to run when the page is unloaded*/
    var d = new Date();
    Qualtrics.SurveyEngine.setEmbeddedData("t_rejectionEssay_submit", d);

});
