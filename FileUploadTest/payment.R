library(shiny)
ui <- fluidPage(tags$script(src = "https://www.paypalobjects.com/api/checkout.js"),
                tags$script("paypal.Button.render({
                            // Configure environment
                            env: 'sandbox',
                            client: {
                            sandbox: 'demo_sandbox_client_id',
                            production: 'demo_production_client_id'
                            },
                            // Customize button (optional)
                            locale: 'en_US',
                            style: {
                            size: 'small',
                            color: 'gold',
                            shape: 'pill',
                            },
                            // Set up a payment
                            payment: function (data, actions) {
                            return actions.payment.create({
                            transactions: [{
                            amount: {
                            total: '0.01',
                            currency: 'USD'
                            }
                            }]
                            });
                            },
                            // Execute the payment
                            onAuthorize: function (data, actions) {
                            return actions.payment.execute()
                            .then(function () {
                            // Show a confirmation message to the buyer
                            window.alert('Thank you for your purchase!');
                            });
                            }
                            }, '#paypal-button');"),
                tags$div(id = "paypal-button"))
server <- function(input, output) {}
shinyApp(ui, server)