from pydantic import BaseModel, ValidationError, validator
from typing import List, Optional
from datetime import datetime


class User(BaseModel):
    id_usr: int
    username: str
    password: str
    alias: Optional[str] = 'anonymous'
    timestamp: Optional[datetime] = None
    friends: List[int] = []

    @validator('id_usr')
    def id_must_be_4_digits(cls, v):
        if len(str(v)) != 4:
            raise ValueError('must be 4 digits')
        return v


data = {'id_usr': 1234,
        'username': 'wai foong',
        'password': 'Password123',
        'timestamp': '2020-08-03 10:30',
        'friends': [1, 2, 3]}

try:
    user = User(**data)
    print(user)
except ValidationError as err:
    print(err.json())
