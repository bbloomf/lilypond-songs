\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Roast Beef of Old England"}}
  poet = \markup\oldStyleNum"Henry Fielding (1707–1754) and others"
  composer = \markup\oldStyleNum"Richard Leveridge (1670–1758)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0.0)
       (minimum-distance . 0.0)
       (padding . 1.25)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key c \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
  \tieDashed
}

sopMusic = \relative c' {
	\partial 8
  g'8 |
  c8. d16 c8 b c d |
  \tieDashed
  e d c d4 
  
  b16~ b |
  c8. d16 c8 b( a) g |
  a d, fis g4
  
  g16~ g |
  \tieSolid
  a8 f a c4 b16 a |
  g8 e c a'4. |
  
  d8. e16 f8 b,8. a16 b8 |
  c4. g4 e8 |
  f8. g16 a8 g c b |
  \tieSolid
  c4.~ c4 \bar"|."
}
sopWords = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"1. "
	When might -- y Roast Beef was the Eng -- lish -- man’s food,
  It en -- no -- bled our hearts and en -- rich -- ed our blood.
  Our __ _ sol -- diers were brave and our court -- iers were good.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  But since we have learned from ef -- fem -- in -- ate France
  To __ eat their ra -- gouts __ as well as to dance,
  We’re fed up with noth -- ing but vain com -- plais -- sance.
}

sopWordsIII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"3. "
  Our fa -- thers of old were ro -- bust, stout, and strong,
  And they kept o -- pen house with good cheer all day long,
  Which _ made their plump ten -- ants re -- joice in this song:
  
  Oh! the Roast Beef of old Eng -- land,
  And oh for old Eng -- land’s Roast Beef! __ _
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  Great Han -- del, ’tis said, could eat din -- ner for six,
  Which was doubt -- less his rea -- son on Eng -- land to fix,
  As the land where good mu -- sic with eat -- ing they mix.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  Then long may each Brit -- on of beef have his fill
  At __ _ Christ -- mas, the sea -- son of peace and good -- will,
  For the man that’s well fed, sirs, can nev -- er do ill.
}

altoMusic = \relative c' {
  g'8 |
  e8. e16 g8 g g b |
  g g g b4 \bar""\break
  
  \tieDashed
  g16~ g |
  fis8. fis16 fis8 g( fis) g |
  fis d c b4 \bar""\break
  
  d16~ d |
  \tieSolid
  f8 f f f4 f16 f |
  e8 c c cis4. \bar"||"\break
  
  f8. g16 a8 f8. f16 f8 |
  e4. c4 c8 |
  d8. e16 f8 f f f |
  e4.~ e4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  g8 |
  g8. g16 c8 d e f |
  c e c f4
  
  \tieDashed
  f16~ f |
  d8. d16 d8 d( c) b |
  c c a d4
  
  b16~ b |
  \tieSolid
  c8 a c a4 g16 f |
  g8 g e e4. |
  
  a8. d16 d8 d8. d16 d8 |
  c4. c4 c8 |
  a8. g16 f8 d' d d |
  c4.~ c4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g8 |
  c,8. c16 e8 g g g |
  c, c e g4
  
  \tieDashed
  g,16~ g |
  a8. a16 a8 g( d') e |
  d fis d g4
  
  g16~ g |
  \tieSolid
  f8 f f f4 f16 f |
  c8 c c a4. |
  
  d8. d16 d8 g8. g16 g8 |
  a4. e4 c8 |
  f8. e16 d8 g g g, |
  c4.~ c4 \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #0.9
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
}


