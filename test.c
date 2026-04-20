{
    int a = 10;
    int b = 15;
    int result = a + b;

    {
        int result = 9;
        {
            int result = 8;
            {
                int result = 7;
                {
                    int result = 6;
                    {
                        int result = 5;
                        {
                            int result = 4;
                            {
                                int result = 3;
                                {
                                    int result = 2;
                                    {
                                        int result = 1;
                                        print result;
                                    };
                                    print result;
                                };
                                print result;
                            };
                            print result;
                        };
                        print result;
                    };
                    print result;
                };
                print result;
            };
            print result;
        };
        print result;
    };

    print result * 5;
};
